#include "Core/ArgsParser.h"
#include <iostream>
#include <assert.h>

void Test(int argc, char **argv)
{
    const char *Windows = "";
    const char *Linux = "";
    const char *Test = "";

    Core::ArgsParser argParser;

    argParser.generateArgument(Windows, "-windows");  // required = false; requires input = true; default input = ""; 
    argParser.generateArgument(Linux, "-linux", false, false, "Maconha"); // required = false; requires input = true; default input = "Maconha";
    argParser.generateArgument(Test, "-test", true, true); // required = true; requires input = true; default input = "";
    argParser.generateArgument(Windows, "-windows_overload", false, false, "Windows has been overloaded!");
    assert(argParser.Parse(argc, argv, true) == Core::parseOutput::po_SUCCESS);

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