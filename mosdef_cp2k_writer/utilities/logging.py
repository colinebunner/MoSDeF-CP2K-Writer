# Custom logger for changes and errors
import datetime
from typing import Any

def errorLogEntry(
        errorMessage: str, errType: str = "", callingModule: str = "", callingVariable: str = "",
        exception: Any = None
):

    """
        Custom error log entry. On top of providing a date and error message, also has fields
        for the type of error, what module the error was triggered in, the variable that
        triggered the error, and a field for storing the exception args if applicable. Note
        that this format was largely developed to handle the type-checking that occurs during
        initialization and update of the input variables stored in the simulation object. Hence,
        for more standard runtime errors encountered and logged, some of the fields are not
        going to be applicable, and likely you will just want to 
            a) Skip exception catching and just terminate the program with full traceback.
            b) Just pass in the exception type (e.g. errType = "TypeError") and the exception.args,
               which will provide the error, but not the full traceback.

        Inputs:
            errorMessage (str): Any helpful string describing the error.
            errType (Optional[str]): Error type. Could be an exception type, or custom. For example,
                Init or Setter describes whether a variable was set incorrectly during initialization
                routine or while trying to update via the setter. "<class 'TypeError'>" gives you a
                json serializable indicator that a TypeError was hit.
            callingModule (Optional[str]): Hint as to the module the error was hit in.
            callingVariable (Optional[str]): Hint as to the variable that cause error logging.
            exception (Optional[Any]): Any more info about the exception (e.g. exception.args)

        Outputs:
            errorDct (dict): Populated errorLog dictionary entry
    """

    return ({
        "Date": datetime.datetime.now(),
        "Type": errType,
        "Module": callingModule,
        "Variable": callingVariable,
        "ErrorMessage": errorMessage,
        "Exception": exception
    })

def changeLogEntry(
    prevVal: Any, newVal: Any, callingModule: str = "", callingVariable: str = "", callingLocation: str = ""
):

    """

    """

    return ({
        "Date": datetime.datetime.now(),
        "Module": callingModule,
        "Variable": callingVariable,
        "Previous": prevVal,
        "New": newVal,
        "Location": callingLocation
    })