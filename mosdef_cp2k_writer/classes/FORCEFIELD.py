import datetime
from mosdef_cp2k_writer.utilities import test_instance as ti
from mosdef_cp2k_writer.classes import SPLINE


BOOL_VALS = [".TRUE.", ".FALSE.", "TRUE", "FALSE"]

PARMTYPE_VALS = ["AMBER", "CHM", "G87", "G96", "OFF"]


def _validate_PARM_FILE_NAME(val, errorLog=[]):
    if ti.is_string(val) or (val is None):
        return val
    else:
        errorMessage = "PARM_FILE_NAME must be string."
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "FORCEFIELD",
                "Variable": "PARM_FILE_NAME",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_PARMTYPE(val, errorLog=[]):
    if val is not None:
        val = str(val).upper()

    if val in PARMTYPE_VALS or (val is None):
        return val
    else:
        errorMessage = "Invalid option for PARMTYPE: {}. Valid options are: {}".format(
            val, PARMTYPE_VALS
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "FORCEFIELD",
                "Variable": "PARMTYPE",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


class FORCEFIELD:
    def __init__(
        self, PARM_FILE_NAME=None, PARMTYPE=None, errorLog=[], changeLog=[], location=""
    ):
        self.__errorLog = errorLog
        self.__changeLog = changeLog
        self.__location = "{}/FORCEFIELD".format(location)
        self.__PARM_FILE_NAME = _validate_PARM_FILE_NAME(
            PARM_FILE_NAME, errorLog=self.__errorLog
        )
        self.__PARMTYPE = _validate_PARMTYPE(PARMTYPE, errorLog=self.__errorLog)
        self.__SPLINE = SPLINE.SPLINE(
            errorLog=self.__errorLog,
            changeLog=self.__changeLog,
            location=self.__location,
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
    def PARMTYPE(self):
        return self.__PARMTYPE

    @property
    def PARM_FILE_NAME(self):
        return self.__PARM_FILE_NAME

    @property
    def SPLINE(self):
        return self.__SPLINE

    @PARMTYPE.setter
    def PARMTYPE(self, val):
        val = str(val).upper()
        if val in PARMTYPE_VALS:
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "FORCEFIELD",
                    "Variable": "PARMTYPE",
                    "Success": True,
                    "Previous": self.__PARMTYPE,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__PARMTYPE = val
        else:
            errorMessage = (
                "Invalid option for PARMTYPE: {}. Valid options are: {}".format(
                    val, PARMTYPE_VALS
                )
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "FORCEFIELD",
                    "Variable": "PARMTYPE",
                    "Success": False,
                    "Previous": self.__PARMTYPE,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "FORCEFIELD",
                    "Variable": " PARMTYPE",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @PARM_FILE_NAME.setter
    def PARM_FILE_NAME(self, val):
        if ti.is_string(val) or (val is None):

            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "FORCEFIELD",
                    "Variable": "PARM_FILE_NAME",
                    "Success": True,
                    "Previous": self.__PARM_FILE_NAME,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__PARM_FILE_NAME = val

        else:
            errorMessage = "Invalid option for PARM_FILE_NAME. It should be a string."
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "FORCEFIELD",
                    "Variable": "PARM_FILE_NAME",
                    "Success": False,
                    "Previous": self.__PARM_FILE_NAME,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "FORCEFIELD",
                    "Variable": "PARM_FILE_NAME",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
