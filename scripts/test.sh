EXECUTABLE="morpheus"

case "$(uname)" in
    "Darwin")
        OS=mac-os;;
    MINGW64_NT-*|MSYS_NT-*)
        OS=windows;;
    *)
        OS=linux
esac

if [ "$OS" == "windows" ]; then
  EXECUTABLE="$EXECUTABLE.exe"
fi

rm -rf out
mkdir -p out

7z e "$EXECUTABLE.zip" -o./out

./out/$EXECUTABLE about

rm -rf out