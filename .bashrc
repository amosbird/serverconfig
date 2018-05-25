# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

umask 022
export PATH=/usr/local/bin:$PATH
# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

export PERL_LOCAL_LIB_ROOT="$PERL_LOCAL_LIB_ROOT:/home/amos/perl5";
export PERL_MB_OPT="--install_base /home/amos/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/amos/perl5";
export PERL5LIB="/home/amos/perl5/lib/perl5:$PERL5LIB";
export PATH="/home/amos/perl5/bin:/home/amos/.cargo/bin:/home/amos/go/bin:/home/amos/scripts:$PATH"
