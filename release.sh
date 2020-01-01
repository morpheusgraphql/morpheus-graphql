# vesrion
export MORPHEUS_VESRION="0.9.0"
export DIST_PATH="./dist"
export MORPHEUS_RELEASE_NAME="morpheus-graphql-$MORPHEUS_VESRION"

# generate sdsit
stack sdist . --tar-dir $DIST_PATH

# unppack prod
cd $DIST_PATH
tar -zxvf $MORPHEUS_RELEASE_NAME.tar.gz
cd $MORPHEUS_RELEASE_NAME
cd ../..