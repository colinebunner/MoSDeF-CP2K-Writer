import datetime
import mosdef_cp2k_writer.utilities as utilities


def _validate_PMCLUS_BOX(val, errorLog=[]):

    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "Invalid option for PMCLUS_BOX: {}. Must be a number.".format(
            val
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BOX_PROBABILITIES",
                "Variable": "PMCLUS_BOX",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_PMHMC_BOX(val, errorLog=[]):

    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "Invalid option for PMHMC_BOX: {}. Must be a number.".format(val)
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BOX_PROBABILITIES",
                "Variable": "PMHMC_BOX",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_PMVOL_BOX(val, errorLog=[]):

    if utilities.is_number(val) or (val is None):
        return val
    else:
        errorMessage = "Invalid option for PMVOL_BOX: {}. Must be a number.".format(val)
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "BOX_PROBABILITIES",
                "Variable": "PMVOL_BOX",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


class BOX_PROBABILITIES:
    def __init__(
        self,
        PMCLUS_BOX=None,
        PMHMC_BOX=None,
        PMVOL_BOX=None,
        errorLog=[],
        changeLog=[],
        location="",
    ):

        self.__errorLog = errorLog
        self.__changeLog = changeLog
        self.__PMCLUS_BOX = _validate_PMCLUS_BOX(PMCLUS_BOX, errorLog=self.__errorLog)
        self.__PMHMC_BOX = _validate_PMHMC_BOX(PMHMC_BOX, errorLog=self.__errorLog)
        self.__PMVOL_BOX = _validate_PMVOL_BOX(PMVOL_BOX, errorLog=self.__errorLog)
        self.__location = "{}/BOX_PROBABILITIES".format(location)

    @property
    def PMCLUS_BOX(self):
        return self.__PMCLUS_BOX

    @property
    def PMHMC_BOX(self):
        return self.__PMHMC_BOX

    @property
    def PMVOL_BOX(self):
        return self.__PMVOL_BOX

    @property
    def errorLog(self):
        return self.__errorLog

    @property
    def changeLog(self):
        return self.__changeLog

    @property
    def location(self):
        return self.__location

    @PMCLUS_BOX.setter
    def PMCLUS_BOX(self, val):

        if utilities.is_number(val) or val is None:
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BOX_PROBABILITIES",
                    "Variable": "PMCLUS_BOX",
                    "Success": True,
                    "Previous": self.__PMCLUS_BOX,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__PMCLUS_BOX = val
        else:
            errorMessage = (
                "Invalid option for PMCLUS_BOX: {}. Should be a number.".format(val)
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BOX_PROBABILITIES",
                    "Variable": "PMCLUS_BOX",
                    "Success": False,
                    "Previous": self.__PMCLUS_BOX,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BOX_PROBABILITIES",
                    "Variable": "PMCLUS_BOX",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @PMHMC_BOX.setter
    def PMHMC_BOX(self, val):

        if utilities.is_number(val) or val is None:
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BOX_PROBABILITIES",
                    "Variable": "PMHMC_BOX",
                    "Success": True,
                    "Previous": self.__PMHMC_BOX,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__PMHMC_BOX = val
        else:
            errorMessage = (
                "Invalid option for PMHMC_BOX: {}. Should be a number.".format(val)
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BOX_PROBABILITIES",
                    "Variable": "PMHMC_BOX",
                    "Success": False,
                    "Previous": self.__PMHMC_BOX,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BOX_PROBABILITIES",
                    "Variable": "PMHMC_BOX",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @PMVOL_BOX.setter
    def PMVOL_BOX(self, val):

        if utilities.is_number(val) or val is None:
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BOX_PROBABILITIES",
                    "Variable": "PMVOL_BOX",
                    "Success": True,
                    "Previous": self.__PMVOL_BOX,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__PMVOL_BOX = val
        else:
            errorMessage = (
                "Invalid option for PMVOL_BOX: {}. Should be a number.".format(val)
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "BOX_PROBABILITIES",
                    "Variable": "PMVOL_BOX",
                    "Success": False,
                    "Previous": self.__PMVOL_BOX,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "BOX_PROBABILITIES",
                    "Variable": "PMVOL_BOX",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
