import datetime
from mosdef_cp2k_writer.utilities import test_instance as ti


BOOL_VALS = [".TRUE.", ".FALSE.", "TRUE", "FALSE"]


def _validate_EMAX_ACCURACY(val, errorLog=[]):
    if ti.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "EMAX_ACCURACY should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "SPLINE",
                "Variable": "EMAX_ACCURACY",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_EMAX_SPLINE(val, errorLog=[]):
    if ti.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "EMAX_SPLINE should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "SPLINE",
                "Variable": "EMAX_SPLINE",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_EPS_SPLINE(val, errorLog=[]):
    if ti.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "EPS_SPLINE should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "SPLINE",
                "Variable": "EPS_SPLINE",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_NPOINTS(val, errorLog=[]):
    if ti.is_integer(val) or (val is None):
        return val
    else:
        errorMessage = "NPOINTS should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "SPLINE",
                "Variable": "NPOINTS",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_R0_NB(val, errorLog=[]):
    if ti.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "R0_NB should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "SPLINE",
                "Variable": "R0_NB",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_RCUT_NB(val, errorLog=[]):
    if ti.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "RCUT_NB should be a number."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "SPLINE",
                "Variable": "RCUT_NB",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_UNIQUE_SPLINE(val, errorLog=[]):
    if val is not None:
        val = str(val).upper()

    if val in BOOL_VALS or (val is None):
        return val
    else:
        errorMessage = (
            "Invalid option for UNIQUE_SPLINE: {}. Valid options are: {}".format(
                val, BOOL_VALS
            )
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "SPLINE",
                "Variable": "UNIQUE_SPLINE",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


class SPLINE:
    def __init__(
        self,
        EMAX_ACCURACY=None,
        EMAX_SPLINE=None,
        EPS_SPLINE=None,
        NPOINTS=None,
        R0_NB=None,
        RCUT_NB=None,
        UNIQUE_SPLINE=None,
        errorLog=[],
        changeLog=[],
        location="",
    ):
        self.__errorLog = errorLog
        self.__changeLog = changeLog
        self.__location = "{}/SPLINE".format(location)
        self.__EMAX_ACCURACY = _validate_EMAX_ACCURACY(
            EMAX_ACCURACY, errorLog=self.__errorLog
        )
        self.__EMAX_SPLINE = _validate_EMAX_SPLINE(
            EMAX_SPLINE, errorLog=self.__errorLog
        )
        self.__EPS_SPLINE = _validate_EPS_SPLINE(EPS_SPLINE, errorLog=self.__errorLog)
        self.__NPOINTS = _validate_NPOINTS(NPOINTS, errorLog=self.__errorLog)
        self.__R0_NB = _validate_R0_NB(R0_NB, errorLog=self.__errorLog)
        self.__RCUT_NB = _validate_RCUT_NB(RCUT_NB, errorLog=self.__errorLog)
        self.__UNIQUE_SPLINE = _validate_UNIQUE_SPLINE(
            UNIQUE_SPLINE, errorLog=self.__errorLog
        )

    @property
    def errorLog(self):
        return self.__errorLog

    @property
    def changeLog(self):
        return self.__changeLog

    @property
    def location(self):
        return self.__location

    @property
    def EMAX_ACCURACY(self):
        return self.__EMAX_ACCURACY

    @property
    def EMAX_SPLINE(self):
        return self.__EMAX_SPLINE

    @property
    def EPS_SPLINE(self):
        return self.__EPS_SPLINE

    @property
    def NPOINTS(self):
        return self.__NPOINTS

    @property
    def R0_NB(self):
        return self.__R0_NB

    @property
    def RCUT_NB(self):
        return self.__RCUT_NB

    @property
    def UNIQUE_SPLINE(self):
        return self.__UNIQUE_SPLINE

    @EMAX_ACCURACY.setter
    def EMAX_ACCURACY(self, val):
        if ti.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "EMAX_ACCURACY",
                    "Success": True,
                    "Previous": self.__EMAX_ACCURACY,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__EMAX_ACCURACY = val
        else:
            errorMessage = "EMAX_ACCURACY must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "EMAX_ACCURACY",
                    "Success": False,
                    "Previous": self.__EMAX_ACCURACY,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "SPLINE",
                    "Variable": "EMAX_ACCURACY",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @EMAX_SPLINE.setter
    def EMAX_SPLINE(self, val):
        if ti.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "EMAX_SPLINE",
                    "Success": True,
                    "Previous": self.__EMAX_SPLINE,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__EMAX_SPLINE = val
        else:
            errorMessage = "EMAX_SPLINE must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "EMAX_SPLINE",
                    "Success": False,
                    "Previous": self.__EMAX_SPLINE,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "SPLINE",
                    "Variable": "EMAX_SPLINE",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @EPS_SPLINE.setter
    def EPS_SPLINE(self, val):
        if ti.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "EPS_SPLINE",
                    "Success": True,
                    "Previous": self.__EPS_SPLINE,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__EPS_SPLINE = val
        else:
            errorMessage = "EPS_SPLINE must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "EPS_SPLINE",
                    "Success": False,
                    "Previous": self.__EPS_SPLINE,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "SPLINE",
                    "Variable": "EPS_SPLINE",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @NPOINTS.setter
    def NPOINTS(self, val):
        if ti.is_integer(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "NPOINTS",
                    "Success": True,
                    "Previous": self.__NPOINTS,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__NPOINTS = val
        else:
            errorMessage = "NPOINTS must be an integer."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "NPOINTS",
                    "Success": False,
                    "Previous": self.__NPOINTS,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "SPLINE",
                    "Variable": "NPOINTS",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @R0_NB.setter
    def R0_NB(self, val):
        if ti.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "R0_NB",
                    "Success": True,
                    "Previous": self.__R0_NB,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__R0_NB = val
        else:
            errorMessage = "R0_NB must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "R0_NB",
                    "Success": False,
                    "Previous": self.__R0_NB,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "SPLINE",
                    "Variable": "R0_NB",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @RCUT_NB.setter
    def RCUT_NB(self, val):
        if ti.is_number(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "RCUT_NB",
                    "Success": True,
                    "Previous": self.__RCUT_NB,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__RCUT_NB = val
        else:
            errorMessage = "RCUT_NB must be a number."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "RCUT_NB",
                    "Success": False,
                    "Previous": self.__RCUT_NB,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "SPLINE",
                    "Variable": "RCUT_NB",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @UNIQUE_SPLINE.setter
    def UNIQUE_SPLINE(self, val):
        val = str(val).upper()
        if val in BOOL_VALS:
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "UNIQUE_SPLINE",
                    "Success": True,
                    "Previous": self.__UNIQUE_SPLINE,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__UNIQUE_SPLINE = val
        else:
            errorMessage = (
                "Invalid option for UNIQUE_SPLINE: {}. Valid options are: {}".format(
                    val, BOOL_VALS
                )
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "SPLINE",
                    "Variable": "UNIQUE_SPLINE",
                    "Success": False,
                    "Previous": self.__UNIQUE_SPLINE,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "SPLINE",
                    "Variable": " UNIQUE_SPLINE",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
