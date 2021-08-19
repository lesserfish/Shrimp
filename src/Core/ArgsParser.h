#pragma once

#include <vector>

namespace Core{
    class ArgsParser
    {
        public:
            ArgsParser();
            ~ArgsParser();
            int Parse(int argc, char **argv);
            void generateArgument(const char *&, const char*);
            void generateArgument(const char *&, const char*, bool);
            void generateArgument(const char *&, const char*, bool, bool, const char* = "");
        private:

        struct Argument
        {
            const char *&output;
            const char* value;
            bool required;
            bool requiresInput;
            const char *defaultInput;
        };

        std::vector<Argument*> argList;
    };
}