rem purpose: local testing of produced bundle at file:///D:/projects/GrimClickerHelper/docs/index.html for example

rem or change this to point to chrome whatever
"C:\Program Files\BraveSoftware\Brave-Browser\Application\brave.exe" --allow-file-access-from-files --bwsi --disable-site-isolation-trials --disable-web-security --guest --user-data-dir="C:\temp"

rem or for live updates as you change the code
rem or use build.cmd run
rem and browse localhost:8080