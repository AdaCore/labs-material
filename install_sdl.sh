if [ $(uname) = "Linux" ]; then
    exit
fi

PREFIX=$1

function exit_if_fail {
   if [ $? != 0 ]; then 
      echo "failure: $1"
      exit -1
   fi
}

if [ ! -d ../tmp ]; then
   mkdir ../tmp
fi

cd ../tmp

mkdir -p $PREFIX/lib && mkdir -p $PREFIX/bin && \
unzip -o ../packages/SDL-devel-1.2.15-mingw32.zip && \
chmod u+x SDL-1.2.15/bin/*.dll && \
cp SDL-1.2.15/lib/*.a $PREFIX/lib && \
cp SDL-1.2.15/lib/*.la $PREFIX/lib && \
cp SDL-1.2.15/bin/*.dll $PREFIX/bin

exit_if_fail "installing SDL"

unzip -o ../packages/SDL_ttf-devel-2.0.11-VC.zip && \
chmod u+x SDL_ttf-2.0.11/lib/x86/*.dll && \
cp SDL_ttf-2.0.11/lib/x86/*.lib $PREFIX/lib && \
cp SDL_ttf-2.0.11/lib/x86/*.dll $PREFIX/bin

exit_if_fail "installing SDL TTF"
