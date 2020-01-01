# generate sdsit
stack sdist . --tar-dir ./prod

# unppack prod
cd ./prod
tar -zxvf morpheus-graphql-0.9.0.tar.gz
cd morpheus-graphql-0.9.0
cd ../..
