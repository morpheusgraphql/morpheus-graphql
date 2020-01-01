# vesrion
export MORPHEUS_VESRION="0.9.0"
export DIST_PATH="./dist"
export MORPHEUS_RELEASE_NAME="morpheus-graphql-$MORPHEUS_VESRION"
export MORPHEUS_RELEASE_TAR="$MORPHEUS_RELEASE_NAME.tar.gz" 

# generate sdsit
stack sdist . --tar-dir $DIST_PATH

# unppack prod
cd $DIST_PATH
tar -zxvf $MORPHEUS_RELEASE_TAR
mv $MORPHEUS_RELEASE_NAME release
rm $MORPHEUS_RELEASE_TAR
cd release
cd ../..