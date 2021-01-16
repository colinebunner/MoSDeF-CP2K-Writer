import datetime
from mosdef_cp2k_writer.utilities import test_instance as ti
from mosdef_cp2k_writer.classes import NOSE
from mosdef_cp2k_writer.classes import GLE


def _validate_RMANGLE(val, errorLog=[]):

    if ti.is_list_of_numbers(val) or (val is None):
        return val
    else:
        errorMessage = (
            "Invalid option for RMANGLE: {}. Must be a list of numbers.".format(val)
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "MOL_DISPLACEMENTS",
                "Variable": "RMANGLE",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_RMBOND(val, errorLog=[]):

    if ti.is_list_of_numbers(val) or (val is None):
        return val
    else:
        errorMessage = (
            "Invalid option for RMBOND: {}. Must be a list of numbers.".format(val)
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "MOL_DISPLACEMENTS",
                "Variable": "RMBOND",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_RMDIHEDRAL(val, errorLog=[]):

    if ti.is_list_of_numbers(val) or (val is None):
        return val
    else:
        errorMessage = (
            "Invalid option for RMDIHEDRAL: {}. Must be a list of numbers.".format(val)
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "MOL_DISPLACEMENTS",
                "Variable": "RMDIHEDRAL",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_RMROT(val, errorLog=[]):

    if ti.is_list_of_numbers(val) or (val is None):
        return val
    else:
        errorMessage = (
            "Invalid option for RMROT: {}. Must be a list of numbers.".format(val)
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "MOL_DISPLACEMENTS",
                "Variable": "RMROT",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


def _validate_RMTRANS(val, errorLog=[]):

    if ti.is_list_of_numbers(val) or (val is None):
        return val
    else:
        errorMessage = (
            "Invalid option for RMTRANS: {}. Must be a list of numbers.".format(val)
        )
        errorLog.append(
            {
                "Date": datetime.datetime.now(),
                "Type": "init",
                "Module": "MOL_DISPLACEMENTS",
                "Variable": "RMTRANS",
                "ErrorMessage": errorMessage,
            }
        )
        raise TypeError


class MOL_DISPLACEMENTS:
    def __init__(
        self,
        RMANGLE=None,
        RMBOND=None,
        RMDIHEDRAL=None,
        RMROT=None,
        RMTRANS=None,
        errorLog=[],
        changeLog=[],
        location="",
    ):

        self.__errorLog = errorLog
        self.__changeLog = changeLog
        self.__RMANGLE = _validate_RMANGLE(RMANGLE, errorLog=self.__errorLog)
        self.__RMBOND = _validate_RMBOND(RMBOND, errorLog=self.__errorLog)
        self.__RMDIHEDRAL = _validate_RMDIHEDRAL(RMDIHEDRAL, errorLog=self.__errorLog)
        self.__RMROT = _validate_RMROT(RMROT, errorLog=self.__errorLog)
        self.__RMTRANS = _validate_RMTRANS(RMTRANS, errorLog=self.__errorLog)

        self.__location = "{}/MOL_DISPLACEMENTS".format(location)

    @property
    def RMANGLE(self):
        return self.__RMANGLE

    @property
    def RMBOND(self):
        return self.__RMBOND

    @property
    def RMDIHEDRAL(self):
        return self.__RMDIHEDRAL

    @property
    def RMROT(self):
        return self.__RMROT

    @property
    def RMTRANS(self):
        return self.__RMTRANS

    @property
    def errorLog(self):
        return self.__errorLog

    @property
    def changeLog(self):
        return self.__changeLog

    @property
    def location(self):
        return self.__location

    @RMANGLE.setter
    def RMANGLE(self, val):

        if val is None or ti.is_list_of_numbers(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMANGLE",
                    "Success": True,
                    "Previous": self.__RMANGLE,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__RMANGLE = val
        else:
            errorMessage = (
                "Invalid option for RMANGLE: {}. Must be a list of numbers".format(val)
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMANGLE",
                    "Success": False,
                    "Previous": self.__RMANGLE,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMANGLE",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @RMBOND.setter
    def RMBOND(self, val):

        if val is None or ti.is_list_of_numbers(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMBOND",
                    "Success": True,
                    "Previous": self.__RMBOND,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__RMBOND = val
        else:
            errorMessage = (
                "Invalid option for RMBOND: {}. Must be a list of numbers".format(val)
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMBOND",
                    "Success": False,
                    "Previous": self.__RMBOND,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMBOND",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @RMDIHEDRAL.setter
    def RMDIHEDRAL(self, val):

        if val is None or ti.is_list_of_numbers(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMDIHEDRAL",
                    "Success": True,
                    "Previous": self.__RMDIHEDRAL,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__RMDIHEDRAL = val
        else:
            errorMessage = (
                "Invalid option for RMDIHEDRAL: {}. Must be a list of numbers".format(
                    val
                )
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMDIHEDRAL",
                    "Success": False,
                    "Previous": self.__RMDIHEDRAL,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMDIHEDRAL",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @RMROT.setter
    def RMROT(self, val):

        if val is None or ti.is_list_of_numbers(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMROT",
                    "Success": True,
                    "Previous": self.__RMROT,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__RMROT = val
        else:
            errorMessage = (
                "Invalid option for RMROT: {}. Must be a list of numbers".format(val)
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMROT",
                    "Success": False,
                    "Previous": self.__RMROT,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMROT",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )

    @RMTRANS.setter
    def RMTRANS(self, val):

        if val is None or ti.is_list_of_numbers(val):
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMTRANS",
                    "Success": True,
                    "Previous": self.__RMTRANS,
                    "New": val,
                    "ErrorMessage": None,
                    "Location": self.__location,
                }
            )
            self.__RMTRANS = val
        else:
            errorMessage = (
                "Invalid option for RMTRANS: {}. Must be a list of numbers".format(val)
            )
            self.__changeLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMTRANS",
                    "Success": False,
                    "Previous": self.__RMTRANS,
                    "New": val,
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
            self.__errorLog.append(
                {
                    "Date": datetime.datetime.now(),
                    "Type": "Setter",
                    "Module": "MOL_DISPLACEMENTS",
                    "Variable": "RMTRANS",
                    "ErrorMessage": errorMessage,
                    "Location": self.__location,
                }
            )
