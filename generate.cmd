cd src/Client
rem we need the && to make sure we only generate if the build succeeds
dotnet build && cd ../.. && dotnet fsi GenerateCode.fsx