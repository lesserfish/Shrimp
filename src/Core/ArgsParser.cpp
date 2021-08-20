#include <iostream>
#include <string>
#include "ArgsParser.h"

namespace Core
{
    ArgsParser::ArgsParser() : argList() {}

    ArgsParser::~ArgsParser()
    {
        while (!argList.empty())
        {
            Argument *arg = argList.at(argList.size() - 1);
            argList.pop_back();
            free(arg);
        }
    }
    void ArgsParser::generateArgument(const char *&output, const char *name, bool required, bool requiresInput, const char *defaultInput)
    {
        Argument *arg = new Argument{output, name, required, requiresInput, defaultInput};
        argList.push_back(arg);
    }
    int ArgsParser::Parse(int argc, char **argv, bool assertArgument)
    {
        std::vector<Argument *> missingParameters = argList;
        int position = 0;
        while (++position < argc)
        {
            const char *val = argv[position];

            bool argFound = false;
            for (int i = 0; i < missingParameters.size(); i++)
            {
                Argument *arg = missingParameters.at(i);
                if (std::string(val) == std::string(arg->name))
                {
                    if (arg->requiresInput)
                    {
                        if (++position >= argc)
                            return parseOutput::po_MISSING_INPUT;
                        
                        arg->output = argv[position];
                        missingParameters.erase(missingParameters.begin() + i);
                        argFound = true;
                        break;
                    }
                    else
                    {
                        arg->output = arg->defaultInput;
                        missingParameters.erase(missingParameters.begin() + i);
                        argFound = true;
                        break;
                    }
                }
            }
            if(!argFound & assertArgument)
                return parseOutput::po_ADDITIONAL_PARAMETER_GIVEN;
        }
        for (int i = 0; i < missingParameters.size(); i++)
        {
            Argument *arg = missingParameters.at(i);
            if (arg->required == true)
                return parseOutput::po_MISSING_REQUIRED_PARAMETER;
        }
        
        return parseOutput::po_SUCCESS;
    }
}