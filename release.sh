# vesrion
export MORPHEUS_VESRION="0.10.1"
export MORPHEUS_RELEASE_NAME="morpheus-graphql-$MORPHEUS_VESRION"
export MORPHEUS_RELEASE_TAR="$MORPHEUS_RELEASE_NAME.tar.gz" 

# rm -r $DIST_PATH
mkdir $DIST_PATH
# generate sdsit
cabal sdist -o $DIST_PATH
rm -r dist-newstyle
# unppack prod
cd $DIST_PATH
tar -zxvf $MORPHEUS_RELEASE_TAR
mv $MORPHEUS_RELEASE_NAME release
rm $MORPHEUS_RELEASE_TAR
cd release
echo $PWD
ls
# cd ../..

cabal update
cabal build --only-dependencies