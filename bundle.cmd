rem dotnet run --project .\fBuild\Build.fsproj -- -t %*
rem cls && dotnet fake build -t cleanbundle && dotnet fake build -t bundle
cls
dotnet run --project .\fBuild\Build.fsproj -- -t Clean
dotnet run --project .\fBuild\Build.fsproj -- -t bundle
