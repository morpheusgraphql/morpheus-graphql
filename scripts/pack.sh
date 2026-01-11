NAME="morpheus-graphql-code-gen"
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

# Build
rm -rf out
mkdir -p out
stack build $NAME
cp "$(stack exec which $EXECUTABLE)" ./out/$EXECUTABLE

if [ "$OS" != "windows" ]; then
  chmod +x ./out/$EXECUTABLE
fi

# Package
cd out
7z a ../$EXECUTABLE.zip .
cd ..
rm -rf out
