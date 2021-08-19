#include "Core/ArgsParser.h"
#include <iostream>
#include <assert.h>

void Test(int argc, char **argv)
{
    const char *Windows = "";
    const char *Linux = "";
    const char *Test = "";

    Core::ArgsParser argParser;

    argParser.generateArgument(Windows, "-windows");
    argParser.generateArgument(Linux, "-linux", false, false, "Maconha");
    argParser.generateArgument(Test, "-test", true, true);

    assert(argParser.Parse(argc, argv));

    std::cout << "Evaluating: " << std::endl;
    std::cout << "Windows: " << Windows << std::endl;
    std::cout << "Linux: " << Linux<< std::endl;
    std::cout << "Test: " << Test<< std::endl;
}
int main(int argc, char **argv)
{
    std::cout << argc << std::endl;
    Test(argc, argv);
    return 0;
}