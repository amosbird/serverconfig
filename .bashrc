# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

stty -ixon 2> /dev/null

# User specific environment and startup programs

export LD_RUN_PATH=/usr/local/lib64:/usr/local/lib
export LD_LIBRARY_PATH=/usr/local/lib64:/usr/local/lib
export LIBRARY_PATH=/usr/local/lib64:/usr/local/lib

export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/usr/local/lib64/pkgconfig:/usr/lib/pkgconfig:/usr/lib64/pkgconfig:$PKG_CONFIG_PATH

export PERL_LOCAL_LIB_ROOT="$PERL_LOCAL_LIB_ROOT:/home/amos/perl5";
export PERL_MB_OPT="--install_base /home/amos/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/amos/perl5";
export PERL5LIB="/home/amos/perl5/lib/perl5:$PERL5LIB";
export PATH="/home/amos/perl5/bin:/home/amos/.cargo/bin:/home/amos/go/bin:/home/amos/scripts:$PATH:/bin:/sbin"
