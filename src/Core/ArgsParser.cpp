#include <iostream>
#include <string>
#include "ArgsParser.h"



namespace Core{
    ArgsParser::ArgsParser() : 
    argList(){}

    ArgsParser::~ArgsParser()
    {
        while(!argList.empty())
        {
            Argument *arg = argList.at(argList.size() - 1);
            argList.pop_back();
            free(arg);
        }
    }
    void ArgsParser::generateArgument(const char *& output, const char* value)
    {
        Argument *arg = new Argument{output, value, false, true, ""};
        argList.push_back(arg);
    }
    void ArgsParser::generateArgument(const char *& output, const char* value, bool required)
    {
        Argument *arg = new Argument{output, value, required, true, ""};
        argList.push_back(arg);
    }
    void ArgsParser::generateArgument(const char *& output, const char* value, bool required, bool requiresInput, const char *defaultInput)
    {
        Argument *arg = new Argument{output, value, required, requiresInput, defaultInput};
        argList.push_back(arg);
    }
    int ArgsParser::Parse(int argc, char **argv)
    {
        std::vector<Argument*> missingParameters = argList;
        if(argc > 1)
        {
            int position = 0;
            while(++position < argc)
            {
                const char *val = argv[position];

                for(int i = 0; i < missingParameters.size(); i++)
                {
                    Argument *arg = missingParameters.at(i);
                    if(std::string(val) == std::string(arg->value))
                    {
                        if(arg->requiresInput)
                        {
                            if(++position >= argc)
                            {
                                return 0;
                            }
                            arg->output = argv[position];
                            missingParameters.erase(missingParameters.begin() + i);
                            break;
                        }
                        else
                        {
                            arg->output = arg->defaultInput;
                            missingParameters.erase(missingParameters.begin() + i);
                            break;
                        }
                    }
                }
            }
        }
        for(int i = 0; i < missingParameters.size(); i++)
        {
            Argument *arg = missingParameters.at(i);
            if(arg->required == true)
                return 0;
        }
        return 1;
    }
}