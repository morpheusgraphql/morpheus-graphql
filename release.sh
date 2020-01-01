# generate sdsit
stack sdist . --tar-dir ./prod

# vesrion
export MORPHEUS_VESRION="0.9.0"
export MORPHEUS_RELEASE_NAME="morpheus-graphql-$MORPHEUS_VESRION"

# unppack prod
cd ./prod
tar -zxvf $MORPHEUS_RELEASE_NAME.tar.gz
cd $MORPHEUS_RELEASE_NAME
cd ../..