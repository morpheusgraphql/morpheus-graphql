
readonly ALERT='\033[0;31m'
readonly STD='\033[0m'
readonly INFO='\033[1;36m'
readonly WARN='\033[1;33m'
readonly SUCCSESS='\033[1;32m'
readonly VERSION='0.1.3'

case "$(uname)" in
    "Darwin")
        OS=mac-os;;
    MINGW64_NT-*|MSYS_NT-*)
        OS=windows;;
    *)
        OS=linux
esac

## install bin
if [ -d "$HOME/bin"  ]; then
  BIN_DIR="$HOME/bin"
elif [ -d "$HOME/.local/bin"  ]; then
  BIN_DIR="$HOME/.local/bin"
else 
  BIN_DIR="$HOME/.local/bin"
  mkdir -p $BIN_DIR
fi

readonly URL="https://github.com/nalchevanidze/hconf/releases/download/$VERSION/hconf-$OS.zip"


if [ -d .hconf-local ]; then
  rm -rf .hconf-local
fi

mkdir .hconf-local
cd .hconf-local

echo "\n${INFO}installing hconf $VERSION ${STD}\n" 
echo " - source: $URL";
curl -o "hconf.zip" -LO "$URL" -s

echo " - extracting"

unzip -q hconf.zip
chmod 777 ./hconf

echo " - copying binary to $BIN_DIR"

cp ./hconf $BIN_DIR/hconf

echo " - clean up"

cd ..  
rm -rf .hconf-local

echo "";

if command -v hconf  
then
  echo "${SUCCSESS}installation succeeded for honf $(hconf version).${STD}";
else 
  echo "add ${WARN}$BIN_DIR${STD} to enviroment PATH to execute hconf.";
fi

echo "";