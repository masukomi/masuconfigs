# ~/.bash_profile: executed by bash(1) for login shells.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/login.defs
#umask 022



#/usr/bin/keychain ~/.ssh/internal_key_b 
#~/.ssh/external_key ~/.ssh/deployed_key
#source ~/.ssh-agent > /dev/null

# include .bashrc if it exists
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi


# the rest of this file is commented out.

# set PATH so it includes user's private bin if it exists
#if [ -d ~/bin ] ; then
#    PATH=~/bin:"${PATH}"
#fi

# do the same with MANPATH
#if [ -d ~/man ]; then
#    MANPATH=~/man${MANPATH:-:}
#    export MANPATH
#fi

# If ssh-agent isn't running for this user, then start it and add a key.
# If ssh-agent is running, then instantiate the environment vars saved
# the last time ssh-agent was run. /proc is your friend.
#ssh_agent_pid_1=`find /proc/[0-9]*/stat -user $LOGNAME -exec grep -h ssh-agent \{\} \; 2>/dev/null| \
#   egrep '^[0-9]+'|head -1|cut -d' ' -f1`
#if [ -z $ssh_agent_pid_1 ]; then
#   eval `/usr/bin/ssh-agent|tee $HOME/.ssh/$HOSTNAME.ssh-agent`
#   /usr/bin/ssh-add /home/krhodes/.ssh/internal_key
#   /usr/bin/ssh-add /home/krhodes/.ssh/external_key
#   /usr/bin/ssh-add /home/krhodes/.ssh/deployed_key
#else
#   . $HOME/.ssh/$HOSTNAME.ssh-agent
#fi


. "$HOME/.cargo/env"

source /Users/kayrhodes/.config/broot/launcher/bash/br
