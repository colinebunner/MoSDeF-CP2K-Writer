from mosdef_cp2k_writer.utilities import test_instance as ti
from mosdef_cp2k_writer.utilities import oneDimArray   as oda

def write_section(module_name, variables):

    '''
        Setting up the modules is a pain by hand since I am type-checking. This is
        a script to take an easier to assemble set of information and write all
        the boiler plate. It assembles the getters/setters. The __init__ is filled
        out, but there are possibly many situations in which you would want to edit
        this.
    '''

    # Generate validator for oneDimArrays
    def oda_validate_template(var_name, ti_func):
        return (
            f"def _validate_{var_name}(val, errorLog=[], mode=\"Setter\")\n"
            f"    if ti.{ti_func}(val):\n"
            "        return oda.listToODA([val])\n"
            "    elif isinstance(val, list):\n"
            f"        if np.all([ti.{ti_func}(v) for v in val]):\n"
            "            return oda.listToODA(val)\n"
            "        else:\n"
            "            errorMessage = f\"Not all list values match oneDimArray type. List values: {val}\"\n"
            "            errorLog.append(\n"
            "                errorLogEntry(\n"
            f"                    errorMessage, errType=mode, callingModule={module_name}, callingVariable={var_name},\n"
            f"                    exception=\"<class 'TypeError'>\"\n"
            "                )"
            "            )\n"
            "    elif isinstance(val, oda.oneDimArray) or (val is None):\n"
            "        return val\n"
            "    else:\n"
            f"        errorMessage = f\"Invalid option for {module_name}/{var_name}: {{val}}\"\n"
            "        errorLog.append(\n"
            "            errorLogEntry(\n"
            f"                errorMessage, errType=mode, callingModule={module_name}, callingVariable={var_name},\n"
            f"                exception=\"<class 'TypeError'>\"\n"
            "            )\n"
            "        )\n\n\n"
        )

    def value_validate_template(var_name, ti_func):
        return (
            f"def _validate_{var_name}(val, errorLog=[], mode=\"Setter\")\n"
            f"    if ti.{ti_func}(val) or (val is None):\n"
            "        return val\n"
            "    else:\n"
            f"        errorMessage = f\"Invalid option for {module_name}/{var_name}: {{val}}\"\n"
            "        errorLog.append(\n"
            "            errorLogEntry(\n"
            f"                errorMessage, errType=mode, callingModule={module_name}, callingVariable={var_name},\n"
            f"                exception=\"<class 'TypeError'>\"\n"
            "            )\n"
            "        )\n\n\n"
        )
    
    def allowed_value_validate_template(var_name, allowed_values):
        return (
            f"def _validate_{var_name}(val, errorLog=[], mode=\"Setter\")\n"
            f"    if val in {allowed_values} or (val is None):\n"
            "        return val\n"
            "    else:\n"
            f"        errorMessage = f\"Invalid option for {module_name}/{var_name}: {{val}}\"\n"
            "        errorLog.append(\n"
            "            errorLogEntry(\n"
            f"                errorMessage, errType=mode, callingModule={module_name}, callingVariable={var_name},\n"
            f"                exception=\"<class 'TypeError'>\"\n"
            "            )\n"
            "        )\n\n\n"
        )

    def setter_template(var_name):
        return(
            f"    @{var_name}.setter\n"
            f"    def {var_name}(self, val):\n"
            f"        returnVal = _validate_{var_name}(val, errorLog=self.__errorLog, mode=\"Setter\")\n"
            f"        if returnVal is not None:\n"
            "            self.__changeLog.append(\n"
            "                changeLogEntry(\n"
            f"                    self.__{var_name}, returnVal, callingModule={module_name}, callingVariable={var_name},\n"
            "                    callingLocation=self.__location\n"
            "                )\n"
            "            )\n"
            "            self.__{var_name} = returnVal\n\n\n"
        )
        
    
    type_2_ti = {
        "number": "is_number",
        "positive_number": "is_positive_number",
        "int": "is_integer",
        "positive_int": "is_positive_integer",
        "probability": "is_probability",
        "boolean": "is_boolean"
    }

    # Writing the code with good, old-fashioned string operations
    init_string = ""    
    init_set = ""
    validate_funcs = ""
    properties = ""
    setters = ""

    # Very poorly 
    for i, v in enumerate(variables):

        nm       = v["name"]
        var_type = v["type"]

        # Add property definition
        properties += f"    @property\n    def {nm}(self):\n        return self.__{nm}\n\n"

        # Add variable name to __init__ signature
        if i == 0:
            init_string += f"{nm}=None"
        elif i%4 == 0:
            init_string += f",{nm}=None\n"
        else:
            init_string += f",{nm}=None"

        # Set __ protected Class instance variable to value after
        # passing through value checker
        init_set += f"        self.__{nm} = _validate_{nm}({nm}, errorLog=self.__errorLog, mode=\"Init\")\n"

        # Add the _validate functions and setters
        if var_type == "oda":
            validate_funcs += oda_validate_template(nm, type_2_ti[v["oda_type"]])
        elif var_type == "allowed_values":
            validate_funcs += allowed_value_validate_template(nm, type_2_ti[v["allowed_values"]])
        else:
            validate_funcs += value_validate_template(nm, type_2_ti[var_type])

        setters += setter_template(nm)        

    code = (
        "from mosdef_cp2k_writer.utilities import test_instance as ti\n"
        "from mosdef_cp2k_writer.utilities import oneDimArray   as oda\n\n" + 
        validate_funcs + 
        f"class {nm.capitalize()}:\n\n"
        "    def __init__(\n"
        "        self," + init_string +
        "        ,errorLog=[],changeLog=[],location=\"\"\n"
        "    ):\n\n" +
        init_set +
        f"        self.__errorLog = errorLog\n"
        f"        self.__changeLog = changeLog\n"
        f"        self.__location = \"{'{}'}/{module_name}\".format(location)\n\n\n" +
        properties +
        setters
    )

    return code