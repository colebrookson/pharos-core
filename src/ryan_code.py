"""
Register parsing and validation module.

A "dataset" in the Pharos UI is a register, which is a dictionary (or
object, in the JS frontend) of records. Each record has a set of optional
fields by default, which will be validated when the register is parsed,
but will also accept fields with any name. Each field is a datapoint,
and when the register is saved to the api, known fields are validated and
each datapoint gets a report which tells the system if it is ready
to import into the Pharos database, and if not, it includes a message
to help the user understand what they need to change. Unknown datapoints
are simply marked with a "WARNING" status.

Datapoints are a recursive data structure, where the `previous` property
points to the previous version of the datapoint. This allows us to track
the history of a datapoint, tracking when it changed, what changed, and
who made that change.

This is the core module which defines datapoints, validation reports,
and records.
"""

from datetime import datetime
from enum import Enum
from functools import wraps
from typing import Any, Dict, Optional, cast

from column_alias import get_ui_name
from pydantic import BaseModel, Extra, Field, validator
from value_alias import (
    DEAD_OR_ALIVE_VALUES_MAP,
    DETECTION_OUTCOME_VALUES_MAP,
    ORGANISM_SEX_VALUES_MAP,
)


class User(BaseModel):
    """The user class which holds metadata in dynamodb."""

    researcher_id: str = Field(..., alias="researcherID")
    """Unique identifier, and used as the partition key in dynamodb."""

    organization: str
    """Org affiliation of the user."""

    email: str
    """The email address of the user."""

    name: str
    """The display-name of the user, shown in the UI."""

    project_ids: Optional[set[str]] = Field(None, alias="projectIDs")
    """The projectIDs of the projects this user can access and edit."""

    first_name: str = Field(None, alias="firstName")
    """The first name of the user."""

    last_name: str = Field(None, alias="lastName")
    """The last name of the user."""

    download_ids: Optional[set[str]] = Field(None, alias="downloadIDs")
    """The set of downloadIDs of the downloads this user has created."""

    class Config:
        extra = Extra.forbid

    def table_item(self):
        """Return the user as a dict, with the researcherID
        as the partition key and the sort key as _meta.
        """
        user_dict = self.dict(by_alias=True)
        user_dict["pk"] = user_dict.pop("researcherID")
        user_dict["sk"] = "_meta"
        return user_dict

    @classmethod
    def parse_table_item(cls, table_item):
        """Parse the user from a MetadataTable item."""
        table_item["researcherID"] = table_item.pop("pk")
        table_item.pop("sk")
        return User.parse_obj(table_item)


class ProjectAuthorRole(str, Enum):
    """The role of the author in the project."""

    ADMIN = "Admin"


class Author(BaseModel):
    researcher_id: str = Field(alias="researcherID")
    role: str


class ProjectPublishStatus(str, Enum):
    """The state the project in the publishing process"""

    UNPUBLISHED = "Unpublished"
    PUBLISHING = "Publishing"
    PUBLISHED = "Published"


class Project(BaseModel):
    """The metadata object which describes a project."""

    project_id: str = Field(alias="projectID")
    name: str
    dataset_ids: list[str] = Field(alias="datasetIDs")
    deleted_dataset_ids: Optional[list[str]] = Field(None, alias="deletedDatasetIDs")
    last_updated: Optional[str] = Field(None, alias="lastUpdated")
    description: Optional[str]
    project_type: Optional[str] = Field(None, alias="projectType")
    surveillance_status: Optional[str] = Field(None, alias="surveillanceStatus")
    citation: Optional[str]
    related_materials: Optional[list[str]] = Field(None, alias="relatedMaterials")
    project_publications: Optional[list[str]] = Field(None, alias="projectPublications")
    others_citing: Optional[list[str]] = Field(None, alias="othersCiting")
    authors: Optional[list[Author]]
    publish_status: ProjectPublishStatus = Field(None, alias="publishStatus")

    class Config:
        extra = Extra.forbid
        use_enum_values = True

    def table_item(self):
        """Return the project as a dict, with the projectID
        as the partition key and the sort key as _meta.
        """
        project_dict = self.dict(by_alias=True)
        project_dict["pk"] = project_dict.pop("projectID")
        project_dict["sk"] = "_meta"
        return project_dict

    @classmethod
    def parse_table_item(cls, table_item):
        """Parse the project from a MetadataTable item."""
        table_item["projectID"] = table_item.pop("pk")
        table_item.pop("sk")
        return Project.parse_obj(table_item)


# complex fields are any fields where two or more
# datapoints are combined to create a single field
# in the PublishedRecord database model.
COMPLEX_FIELDS = {
    # date component fields
    "collection_day",
    "collection_month",
    "collection_year",
    # location component fields
    "latitude",
    "longitude",
}

# Fields requried to release a dataset
# component fields of complex fields
# are always required
REQUIRED_FIELDS = COMPLEX_FIELDS.union(
    {
        "host_species",
    }
)


class DatasetReleaseStatus(str, Enum):
    """The state the dataset in the release process"""

    UNRELEASED = "Unreleased"
    RELEASING = "Releasing"
    RELEASED = "Released"
    PUBLISHED = "Published"
    PUBLISHING = "Publishing"


class Version(BaseModel):
    """Version objects, which represent specific
    timestamps within a register which the user
    may want to refer back to. This has been
    largely removed from the user interface."""

    date: str
    name: str


class RegisterPage(BaseModel):
    """Object for tracking pages of datasets, used for
    paginated storage and processing."""

    last_updated: str = Field(None, alias="lastUpdated")


class ReleaseReport(BaseModel):
    release_status: DatasetReleaseStatus = Field(
        default=DatasetReleaseStatus.UNRELEASED, alias="releaseStatus"
    )
    success_count: int = Field(default=0, alias="successCount")
    warning_count: int = Field(default=0, alias="warningCount")
    fail_count: int = Field(default=0, alias="failCount")
    missing_count: int = Field(default=0, alias="missingCount")
    warning_fields: dict[str, list] = Field(default={}, alias="warningFields")
    fail_fields: dict[str, list] = Field(default={}, alias="failFields")
    missing_fields: dict[str, list] = Field(default={}, alias="missingFields")

    @classmethod
    def merge(cls, left: "ReleaseReport", right: "ReleaseReport"):
        next = ReleaseReport()

        if (
            # set RELEASED if both reports agree
            left.release_status == DatasetReleaseStatus.RELEASED
            and right.release_status == DatasetReleaseStatus.RELEASED
        ):
            next.release_status = DatasetReleaseStatus.RELEASED

        next.success_count = left.success_count + right.success_count
        next.warning_count = left.warning_count + right.warning_count
        next.fail_count = left.fail_count + right.fail_count
        next.missing_count = left.missing_count + right.missing_count

        next.warning_fields = left.warning_fields | right.warning_fields
        next.fail_fields = left.fail_fields | right.fail_fields
        next.missing_fields = left.missing_fields | right.missing_fields

        return next


class Dataset(BaseModel):
    """The dataset object which contains
    metadata about the dataset.
    """

    project_id: str = Field(alias="projectID")
    """The projectID of the project to which this dataset
    belongs; used as the partition key in dynamodb."""

    dataset_id: str = Field(alias="datasetID")
    """Unique identifier for the dataset; used as the sort key."""

    name: str
    """The display-name of the dataset, shown in the UI."""

    last_updated: Optional[str] = Field(None, alias="lastUpdated")
    """lastUpdated is the timestamp of the last time any datapoint
    in the dataset was updated."""

    earliest_date: Optional[str] = Field(None, alias="earliestDate")
    """The earliest date in the dataset, as a string. This is
    used to display and sort the datasets in the UI."""

    latest_date: Optional[str] = Field(None, alias="latestDate")
    """The latest date in the dataset, as a string. This is
    used to display and sort the datasets in the UI."""

    release_status: Optional[DatasetReleaseStatus] = Field(None, alias="releaseStatus")
    """Whether the dataset is unreleased, released, or published."""

    release_report: Optional[ReleaseReport] = Field(alias="releaseReport")

    register_pages: Optional[Dict[str, RegisterPage]] = Field(
        None, alias="registerPages"
    )
    """Dictionary of metadata for paginated register storage and processing"""

    age: Optional[str]
    """The user-selected units for the age field"""

    mass: Optional[str]
    """The user-selected units for the mass field"""

    length: Optional[str]
    """The user-selected units for the length field"""

    class Config:
        extra = Extra.forbid
        use_enum_values = True

    def table_item(self):
        """Return the dataset as a dict structured as a
        metadata table item, with the projectID as the
        partition key and the datasetID as the sort key.
        """
        dataset_dict = self.dict(by_alias=True)

        # remove projectID and datasetID from the dict and
        # use the values for the pk and sk attributes
        dataset_dict["pk"] = dataset_dict.pop("projectID")
        dataset_dict["sk"] = dataset_dict.pop("datasetID")
        return dataset_dict

    @classmethod
    def parse_table_item(cls, table_item):
        """Parse the dataset from a MetadataTable item."""
        table_item["projectID"] = table_item.pop("pk")
        table_item["datasetID"] = table_item.pop("sk")
        return Dataset.parse_obj(table_item)


class ReportScore(Enum):
    """The score of a validated datapoint, used for
    inclusion criteria into the final database.
    """

    FAIL = "FAIL"
    SUCCESS = "SUCCESS"
    WARNING = "WARNING"

    class Config:  # pylint: disable=too-few-public-methods
        extra = Extra.forbid


class Report(BaseModel):
    """The report object for a validated datapoint,
    containing both the score and a message to
    show the user in the frontend.
    """

    status: ReportScore
    message: str
    data: Optional[dict[str, Any]] = None

    class Config:
        extra = Extra.forbid


class Datapoint(BaseModel):
    """The datapoint object, the main unit of
    data in the register system. It contains
    the data, metadata, and the recursive
    history of the datapoint.
    """

    data_value: str = Field(..., alias="dataValue")
    """The value of the datapoint, as a string.
    In most cases this is the raw value the user entered,
    but in some cases it is a transformed from the user's
    selected unit into SI units.
    """

    modified_by: str = Field(..., alias="modifiedBy")
    """The researcherID of the user who modified this
    version of this datapoint.
    """

    version: int
    """The version of this datapoint, which is an integer timestamp
    used to reconcile divergent datapoint histories.
    """

    report: Optional[Report] = None
    """The report for this datapoint, once it has been validated."""

    previous: Optional["Datapoint"] = None
    """The previous version of this datapoint, if it exists."""

    def __float__(self):
        """Return the datapoint's data_value as an float, or if
        data_value cannot be converted to an float, raise a
        ValueError exception.
        """
        try:
            return float(self.data_value)
        except ValueError as e:
            raise ValueError("Value must be a number") from e

    def __str__(self):
        return str(self.data_value)

    def __int__(self):
        """Return the datapoint's data_value as an int, or if
        data_value cannot be converted to an int raise a
        ValueError exception.
        """
        try:
            return int(self.data_value)
        except ValueError as e:
            raise ValueError("Value must be an integer") from e

    def __len__(self):
        return len(self.data_value)

    def nonzero_int(self):
        """Return the datapoint's non-zero data_value as an
        int, and if data_value cannot be converted to an int
        int or is equal to zero, raise a ValueError exception.
        """
        if int(self) == 0:
            raise ValueError("Value must be a non-zero integer")

        return int(self)

    def isnumeric(self):
        """Check if data_value is numeric."""
        return self.data_value.isnumeric()

    @classmethod
    def merge(cls, left: "Datapoint | None", right: "Datapoint | None"):
        """Given two versions of the same datapoint with differing histories,
        return a merged single datapoint with one chronological history."""
        if left is None:
            return right
        if right is None:
            return left

        # If version is a perfect match, keep
        # the datapoint which has a report
        if left.version == right.version:
            if left.report:
                next = left.copy()
                next.previous = Datapoint.merge(left.previous, right.previous)
                return next
            if right.report:
                next = right.copy()
                next.previous = Datapoint.merge(left.previous, right.previous)
                return next
            # if there is no report to preserve, they are considered
            # identical so it doesn't matter which we keep.
            return left

        if left.version > right.version:
            next = left.copy()
            next.previous = Datapoint.merge(left.previous, right)
            return next

        next = right.copy()
        next.previous = Datapoint.merge(left, right.previous)
        return next

    class Config:
        extra = Extra.forbid


class DefaultPassDatapoint(Datapoint):
    """A Datapoint which automatically adds a passing
    report as long as the `Datapoint` is correctly formatted
    and structured. This initial report is often overridden
    by additional validation rules in the Record validator.
    """

    def __init__(self, **data) -> None:
        super().__init__(**data)
        if not self.report and self.data_value != "":
            self.report = Report(
                status=ReportScore.SUCCESS, message="Ready to release."
            )


## decorator to make the validator skip datapoints which
## already have a failure or warning, because they're already invalid
def validator_skip_fail_warn(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        if args[1].report and args[1].report.status in (
            ReportScore.FAIL,
            ReportScore.WARNING,
        ):
            return args[1]
        return func(*args, **kwargs)

    return wrapper


## If the datavalue is an empty string,
## set the report to None and skip further
## validation. These datapoints are valid
## and important because they include history,
## but should not be validated or included in
## the PublishedRecord.
def validator_skip_empty_string(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        if args[1].data_value == "":
            args[1].report = None
            return args[1]
        return func(*args, **kwargs)

    return wrapper


class RecordMeta(BaseModel):
    """An object for storing metadata about a
    record, such as the user's desired default
    order of the records."""

    order: int


class Record(BaseModel):
    """The record object is displayed as a "row"
    to the user in the user interface, and closely
    approximates a row in the Pharos database.

    This model is used to both parse and validate
    the data; validation errors are added back to
    individual datapoints as a Report object for
    display in the user interface.
    """

    meta: Optional[RecordMeta] = Field(alias="_meta")
    sample_id: Optional[DefaultPassDatapoint] = None
    animal_id: Optional[DefaultPassDatapoint] = None
    host_species: Optional[DefaultPassDatapoint] = None
    host_species_ncbi_tax_id: Optional[DefaultPassDatapoint] = None
    latitude: Optional[DefaultPassDatapoint] = None
    longitude: Optional[DefaultPassDatapoint] = None
    spatial_uncertainty: Optional[DefaultPassDatapoint] = None
    collection_day: Optional[Datapoint] = None
    collection_month: Optional[Datapoint] = None
    collection_year: Optional[Datapoint] = None
    collection_method_or_tissue: Optional[DefaultPassDatapoint] = None
    detection_method: Optional[DefaultPassDatapoint] = None
    primer_sequence: Optional[DefaultPassDatapoint] = None
    primer_citation: Optional[DefaultPassDatapoint] = None
    detection_target: Optional[DefaultPassDatapoint] = None
    detection_target_ncbi_tax_id: Optional[DefaultPassDatapoint] = None
    detection_outcome: Optional[DefaultPassDatapoint] = None
    detection_measurement: Optional[DefaultPassDatapoint] = None
    detection_measurement_units: Optional[DefaultPassDatapoint] = None
    pathogen: Optional[DefaultPassDatapoint] = None
    pathogen_ncbi_tax_id: Optional[DefaultPassDatapoint] = None
    genbank_accession: Optional[DefaultPassDatapoint] = None
    detection_comments: Optional[DefaultPassDatapoint] = None
    organism_sex: Optional[DefaultPassDatapoint] = None
    dead_or_alive: Optional[DefaultPassDatapoint] = None
    health_notes: Optional[DefaultPassDatapoint] = None
    life_stage: Optional[DefaultPassDatapoint] = None
    age: Optional[DefaultPassDatapoint] = None
    mass: Optional[DefaultPassDatapoint] = None
    length: Optional[DefaultPassDatapoint] = None

    @validator("host_species")
    @validator_skip_fail_warn
    @validator_skip_empty_string
    def check_host_species(cls, host_species: DefaultPassDatapoint):
        disallowed_species = ["homo sapiens", "homo sapien", "human"]
        if str(host_species).lower() in disallowed_species:
            host_species.report = Report(
                status=ReportScore.FAIL,
                message="Please do not upload data on human infections to Pharos.",
            )

        return host_species

    @validator(
        "host_species_ncbi_tax_id",
        "detection_target_ncbi_tax_id",
        "pathogen_ncbi_tax_id",
    )
    @validator_skip_fail_warn
    @validator_skip_empty_string
    def check_ncbi(cls, ncbi_id: DefaultPassDatapoint):
        """Check that the NCBI taxonomic identifier is
        numeric and between 1 and 7 digits long.
        """
        try:
            if int(ncbi_id) and not 0 < len(ncbi_id) < 8:
                ncbi_id.report = Report(
                    status=ReportScore.FAIL,
                    message="A NCBI taxonomic identifier consists of one to seven digits.",
                )
        except ValueError as e:
            ncbi_id.report = Report(status=ReportScore.FAIL, message=str(e))

        return ncbi_id

    @validator("detection_outcome")
    @validator_skip_fail_warn
    @validator_skip_empty_string
    def check_detection_outcome(cls, detection_outcome: DefaultPassDatapoint):
        """Check that the detection outcome is a valid value."""
        try:
            if str(detection_outcome).lower() not in DETECTION_OUTCOME_VALUES_MAP:
                detection_outcome.report = Report(
                    status=ReportScore.FAIL,
                    message=(
                        "Detection outcome must be an "
                        "unambiguous value such as 'positive', "
                        "'negative', or 'inconclusive'."
                    ),
                )

        except ValueError as e:
            detection_outcome.report = Report(status=ReportScore.FAIL, message=str(e))

        return detection_outcome

    @validator("organism_sex")
    @validator_skip_fail_warn
    @validator_skip_empty_string
    def check_organism_sex(cls, organism_sex: DefaultPassDatapoint):
        """Check if organism sex is a valid value."""
        try:
            if str(organism_sex).lower() not in ORGANISM_SEX_VALUES_MAP:
                organism_sex.report = Report(
                    status=ReportScore.FAIL,
                    message=(
                        "Organism sex must be an "
                        "unambiguous value such as "
                        "male, female, or unknown."
                    ),
                )

        except ValueError as e:
            organism_sex.report = Report(status=ReportScore.FAIL, message=str(e))

        return organism_sex

    @validator("dead_or_alive")
    @validator_skip_fail_warn
    @validator_skip_empty_string
    def check_dead_or_alive(cls, dead_or_alive: DefaultPassDatapoint):
        """Check if dead or alive is a valid value."""
        try:
            if str(dead_or_alive).lower() not in DEAD_OR_ALIVE_VALUES_MAP:
                dead_or_alive.report = Report(
                    status=ReportScore.FAIL,
                    message=(
                        "Dead or alive must be an "
                        "unambiguous value such as "
                        "dead, alive, or unknown."
                    ),
                )

        except ValueError as e:
            dead_or_alive.report = Report(status=ReportScore.FAIL, message=str(e))

        return dead_or_alive

    @validator("latitude")
    @validator_skip_fail_warn
    @validator_skip_empty_string
    def check_lat(cls, latitude: DefaultPassDatapoint):
        """Check that the latitude is numeric and between -90 and 90."""
        try:
            if not -90 <= float(latitude) <= 90:
                latitude.report = Report(
                    status=ReportScore.FAIL,
                    message="Latitude must be between -90 and 90.",
                )

        except ValueError as e:
            latitude.report = Report(status=ReportScore.FAIL, message=str(e))

        return latitude

    @validator("longitude")
    @validator_skip_fail_warn
    @validator_skip_empty_string
    def check_lon(cls, longitude: DefaultPassDatapoint):
        """Check that the longitude is numeric and between -180 and 180."""
        try:
            if not -180 <= float(longitude) <= 180:
                longitude.report = Report(
                    status=ReportScore.FAIL,
                    message="Longitude must be between -180 and 180.",
                )

        except ValueError as e:
            longitude.report = Report(status=ReportScore.FAIL, message=str(e))

        return longitude

    @validator("collection_year")
    @validator_skip_empty_string
    def check_date(cls, year: Datapoint, values: Dict[str, Datapoint]):
        """Check that the date is valid; skip validation if any of
        day, month, and year are missing, and check that the year is
        four digits long."""

        day = values.get("collection_day")
        month = values.get("collection_month")

        # Don't do any validation until all three are filled out
        if not day or not month:
            return year

        if len(year) < 4:
            year.report = Report(
                status=ReportScore.FAIL,
                message="Year must be a four-digit year",
            )
            return year

        try:
            date = datetime(int(year), int(month), int(day))
            report = Report(
                status=ReportScore.SUCCESS,
                message=f"Date {date.strftime('%Y-%m-%d')} is ready to release",
            )

        except ValueError as e:
            try:
                report = Report(
                    status=ReportScore.FAIL,
                    message=f"Date {int(year)}-{int(month)}-{int(day)} is invalid, {e}.",
                )
            except ValueError:
                report = Report(
                    status=ReportScore.FAIL,
                    message="All of day, month, and year must be numbers.",
                )

        day.report, month.report, year.report = report, report, report

        return year

    @validator("age", "mass", "length", "spatial_uncertainty")
    @validator_skip_fail_warn
    @validator_skip_empty_string
    def check_float(cls, value: DefaultPassDatapoint):
        """Check that the value is a decimal."""
        try:
            float(value)
        except ValueError:
            value.report = Report(
                status=ReportScore.FAIL,
                message=(
                    "Must be a number, units can be configured "
                    "in dataset settings (coming soon)."
                ),
            )

        return value

    class Config:
        ## datapoint names are transformed using
        ## a map between snake_case and UI names
        alias_generator = get_ui_name
        extra = Extra.allow

    def __init__(self, **data) -> None:
        super().__init__(**data)
        # Parse any unrecognized fields as Datapoints and add a warning report
        extra_fields = set(self.__dict__) - set(self.__fields__)
        for key in extra_fields:
            dat = Datapoint(**self.__dict__[key])
            dat.report = Report(
                status=ReportScore.FAIL, message="Column is not recognized."
            )
            self.__dict__[key] = dat

    def __iter__(self):  # pyright: ignore [reportIncompatibleMethodOverride]
        iterable: Dict[str, Datapoint | RecordMeta] = self.__dict__
        return iter(iterable.items())

    @classmethod
    def merge(cls, left: "Record | None", right: "Record | None"):
        """Given two versions of the same record, iterate all fields and
        merge all datapoints in both records. If both are None return None"""
        if right is None:
            return left
        if left is None:
            return right

        next = Record()

        for field in cls.__fields__:
            if field != "meta":
                setattr(
                    next,
                    field,
                    Datapoint.merge(getattr(left, field), getattr(right, field)),
                )

        return next


class Register(BaseModel):
    """The register object is the top-level
    object in the register system, containing
    the dictionary of all the records.
    """

    register_data: Dict[str, Record] = Field(alias="register")

    def get_release_report(self) -> ReleaseReport:
        """The release report summarizes all errors and
        warnings in the register, and adds additional
        information such as missing required fields which
        we want to test and display only when the user
        tries to release the dataset.
        """
        report = ReleaseReport()

        for record_id, record in self.register_data.items():
            for field in REQUIRED_FIELDS:
                if (
                    record.__dict__[field] is None
                    ## data_value can be an empty string if the datapoint
                    ## was edited and then "cleared" in the UI
                    or record.__dict__[field].data_value == ""
                ):
                    report.missing_count += 1
                    if record_id not in report.missing_fields:
                        report.missing_fields[record_id] = []
                    report.missing_fields[record_id].append(get_ui_name(field))

            for field, datapoint in record:
                if field == "meta":
                    continue

                datapoint = cast(Datapoint, datapoint)

                if (
                    datapoint is None
                    or datapoint.report is None
                    or datapoint.data_value == ""
                ):
                    # We can skip fields with no reports at this point
                    # because the only case where a field should not
                    # have a report after validation is when that report
                    # depends on another field, and that case will be
                    # caught by the missing fields check above.

                    # This line will be marked as "not covered" in
                    # coverage reports run in python 3.9, because the
                    # cpython compiler optimizes away the statement.
                    # It will be marked as covered in python 3.10.
                    # https://github.com/nedbat/coveragepy/issues/198

                    # I'm intentionally not adding something here (like
                    # a print statement) to make it show up as covered
                    # because the optimization is correct.
                    continue

                if datapoint.report.status == ReportScore.SUCCESS:
                    report.success_count += 1
                    continue

                if datapoint.report.status == ReportScore.WARNING:
                    report.warning_count += 1
                    if record_id not in report.warning_fields:
                        report.warning_fields[record_id] = []
                    report.warning_fields[record_id].append(get_ui_name(field))
                    continue

                if datapoint.report.status == ReportScore.FAIL:
                    report.fail_count += 1
                    if record_id not in report.fail_fields:
                        report.fail_fields[record_id] = []
                    report.fail_fields[record_id].append(get_ui_name(field))

        if (
            report.missing_count == 0
            and report.fail_count == 0
            and report.warning_count == 0
        ):
            report.release_status = DatasetReleaseStatus.RELEASED

        return report