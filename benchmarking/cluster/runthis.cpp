#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <dirent.h>
#include <fcntl.h>
#include <iostream>
#include <vector>
#include <string>
#include <cstring>

std::string outputBase = "";

std::string translateInputToOutputPath(const std::string& inputPath, const std::string& proverName) {
    int index = 0;
    for(int i = 0; i < 5; i++) {
        index = inputPath.find('/', index + 1);
    }
    std::cout << inputPath << ' ' << proverName << ' ' << outputBase << '\n' <<  outputBase + proverName + inputPath.substr(index) << '\n';
    return outputBase + proverName + inputPath.substr(index);
}

int startProcess(const char* path, char* args[], const char* outFilePath)
{
    int pid = fork();
    if(pid == 0) 
    {
        int fd = open(outFilePath, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP);
        // redirect stdout and stderr to file
        dup2(fd, 1);
        dup2(fd, 2);

        close(fd);

        execv(path, args);
    }
    return pid;
}

std::vector<std::string> prepareDirectoriesForOutput(const std::string& name, int level, const std::string& proverName)
{
    std::cout << "preparing directory: " << name << '\n';
    DIR* dir;
    struct dirent* entry;
    std::vector<std::string> allFiles;

    if (!(dir = opendir(name.c_str())))
        return { name };
    if (!(entry = readdir(dir)))
        return { name };

    do {
        if (entry->d_type == DT_DIR) {
            std::string path = name + "/" + entry->d_name;
            if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0)
                continue;
            int index = 0;
            for(int i = 0; i < 2; i++) {
                index = path.find('/', index);
            }
            std::cout << "Creating directory " << outputBase + (proverName + path.substr(index)) << " " <<
            mkdir(translateInputToOutputPath(path, proverName).c_str(), S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH) << "\n";
            auto files = prepareDirectoriesForOutput(path, level + 1, proverName);
            allFiles.insert(allFiles.end(), files.begin(), files.end());
        }
        else
            allFiles.push_back(std::string(name) + "/" + entry->d_name);
    } while (entry = readdir(dir));
    closedir(dir);
    return allFiles;
}

int main(int argc, char** argv)
{
    if(argc < 4) return -1;
    outputBase = argv[3];
    std::string proverLocation = argv[1];
    std::string proverName = proverLocation.substr(proverLocation.rfind('/')+1);
    std::string directory = argv[2];
    std::vector<char*> parameters;
    for(int i = 4; argv[i] != 0; i++) parameters.push_back(argv[i]);
    mkdir((outputBase + proverName).c_str(), S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
    auto allFiles = prepareDirectoriesForOutput(directory, 0, proverName);

    const int numChildren = 64;
    int pids[numChildren] = { 0 };
    for(int i = 0; i < allFiles.size(); i += numChildren)
    {
        std::cout << "Starting batch of processes...\n";
        for(int j = 0; j < numChildren && i + j < allFiles.size(); j++)
        {
            std::vector<char*> arguments;
            arguments.push_back(argv[1]);
            arguments.insert(arguments.begin()+1, parameters.begin(), parameters.end());
            arguments.push_back(const_cast<char*>(allFiles[i + j].c_str()));
            arguments.push_back(0);
            pids[j] = startProcess(proverLocation.c_str(), &arguments[0], (translateInputToOutputPath(allFiles[i+j], proverName) + ".proof").c_str());
        }
        std::cout << "Waiting for batch to finish...\n";
        for(int j = 0; j < numChildren && i + j < allFiles.size(); j++)
        {
            waitpid(pids[j], 0, 0);
        }
        std::cout << "Batch finished.\n";
     
    }
    std::cout << "finished!" << std::endl;
}
    