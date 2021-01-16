import datetime
from mosdef_cp2k_writer.utilities import test_instance as ti


def _validate_PMAVBMC_MOL(val, errorLog=[]):

    if ti.is_list_of_numbers(val) or (val is None):
        return val
    else:
        errorMessage = (
            "Invalid option for PMAVBMC_MOL: {}. Valid options are: {}".format(val)
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "MOL_PROBABILITIES",
                "Variable": "PMAVBMC_MOL",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_PMROT_MOL(val, errorLog=[]):

    if ti.is_list_of_numbers(val) or (val is None):
        return val
    else:
        errorMessage = "Invalid option for PMROT_MOL: {}. Valid options are: {}".format(
            val
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "MOL_PROBABILITIES",
                "Variable": "PMROT_MOL",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_PMSWAP_MOL(val, errorLog=[]):

    if ti.is_list_of_numbers(val) or (val is None):
        return val
    else:
        errorMessage = (
            "Invalid option for PMSWAP_MOL: {}. Valid options are: {}".format(val)
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "MOL_PROBABILITIES",
                "Variable": "PMSWAP_MOL",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_PMTRAION_MOL(val, errorLog=[]):

    if ti.is_list_of_numbers(val) or (val is None):
        return val
    else:
        errorMessage = (
            "Invalid option for PMTRAION_MOL: {}. Valid options are: {}".format(val)
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "MOL_PROBABILITIES",
                "Variable": "PMTRAION_MOL",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_PMTRANS_MOL(val, errorLog=[]):

    if ti.is_list_of_numbers(val) or (val is None):
        return val
    else:
        errorMessage = (
            "Invalid option for PMTRANS_MOL: {}. Valid options are: {}".format(val)
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "MOL_PROBABILITIES",
                "Variable": "PMTRANS_MOL",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


class MOL_PROBABILITIES:
    def __init__(
        self,
        PMAVBMC_MOL=None,
        PMROT_MOL=None,
        PMSWAP_MOL=None,
        PMTRAION_MOL=None,
        PMTRANS_MOL=None,
        errorLog=[],
        changeLog=[],
        location="",
    ):

        self.__errorLog = errorLog
        self.__changeLog = changeLog
        self.__PMAVBMC_MOL = _validate_PMAVBMC_MOL(
            PMAVBMC_MOL, errorLog=self.__errorLog
        )
        self.__PMROT_MOL = _validate_PMROT_MOL(PMROT_MOL, errorLog=self.__errorLog)

        self.__PMSWAP_MOL = _validate_PMSWAP_MOL(PMSWAP_MOL, errorLog=self.__errorLog)
        self.__PMTRAION_MOL = _validate_PMTRAION_MOL(
            PMTRAION_MOL, errorLog=self.__errorLog
        )
        self.__PMTRANS_MOL = _validate_PMTRANS_MOL(
            PMTRANS_MOL, errorLog=self.__errorLog
        )

        self.__location = "{}/MOL_PROBABILITIES".format(location)

    @property
    def PMAVBMC_MOL(self):
        return self.__PMAVBMC_MOL

    @property
    def PMROT_MOL(self):
        return self.__PMROT_MOL

    @property
    def PMSWAP_MOL(self):
        return self.__PMSWAP_MOL

    @property
    def PMTRAION_MOL(self):
        return self.__PMTRAION_MOL

    @property
    def PMTRANS_MOL(self):
        return self.__PMTRANS_MOL

    @property
    def errorLog(self):
        return self.__errorLog

    @property
    def changeLog(self):
        return self.__changeLog

    @property
    def location(self):
        return self.__location

    @PMAVBMC_MOL.setter
    def PMAVBMC_MOL(self, val):

        if val is None or ti.is_list_of_numbers(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMAVBMC_MOL",
                    "Success": True,
                    "Previous": self.__PMAVBMC_MOL,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__PMAVBMC_MOL = val
        else:
            errorMessage = (
                "Invalid option for PMAVBMC_MOL: {}. Must be a list of numbers.".format(
                    val
                )
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMAVBMC_MOL",
                    "Success": False,
                    "Previous": self.__PMAVBMC_MOL,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMAVBMC_MOL",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @PMROT_MOL.setter
    def PMROT_MOL(self, val):

        if val is None or ti.is_list_of_numbers(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMROT_MOL",
                    "Success": True,
                    "Previous": self.__PMROT_MOL,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__PMROT_MOL = val
        else:
            errorMessage = (
                "Invalid option for PMROT_MOL: {}. Must be a list of numbers.".format(
                    val
                )
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMROT_MOL",
                    "Success": False,
                    "Previous": self.__PMROT_MOL,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMROT_MOL",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @PMSWAP_MOL.setter
    def PMSWAP_MOL(self, val):

        if val is None or ti.is_list_of_numbers(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMSWAP_MOL",
                    "Success": True,
                    "Previous": self.__PMSWAP_MOL,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__PMSWAP_MOL = val
        else:
            errorMessage = (
                "Invalid option for PMSWAP_MOL: {}. Must be a list of numbers.".format(
                    val
                )
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMSWAP_MOL",
                    "Success": False,
                    "Previous": self.__PMSWAP_MOL,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMSWAP_MOL",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @PMTRAION_MOL.setter
    def PMTRAION_MOL(self, val):

        if val is None or ti.is_list_of_numbers(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMTRAION_MOL",
                    "Success": True,
                    "Previous": self.__PMTRAION_MOL,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__PMTRAION_MOL = val
        else:
            errorMessage = "Invalid option for PMTRAION_MOL: {}. Must be a list of numbers.".format(
                val
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMTRAION_MOL",
                    "Success": False,
                    "Previous": self.__PMTRAION_MOL,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMTRAION_MOL",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @PMTRANS_MOL.setter
    def PMTRANS_MOL(self, val):

        if val is None or ti.is_list_of_numbers(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMTRANS_MOL",
                    "Success": True,
                    "Previous": self.__PMTRANS_MOL,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__PMTRANS_MOL = val
        else:
            errorMessage = (
                "Invalid option for PMTRANS_MOL: {}. Must be a list of numbers.".format(
                    val
                )
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMTRANS_MOL",
                    "Success": False,
                    "Previous": self.__PMTRANS_MOL,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "MOL_PROBABILITIES",
                    "Variable": "PMTRANS_MOL",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
