function cmakebuild
    mkdir -p build
    if test (count $argv) -eq 1
       export CC=/usr/bin/clang
       export CXX=/usr/bin/clang++
    end
    if test ! -f build/CMakeCache.txt
        cmake -H. -Bbuild ;or rm build/CMakeCache.txt
    end
    if test -f build/CMakeCache.txt
        cmake --build build -- -j(nproc)
    end
end
