//Why do I even bother?

package levin

object S2EBootstrap {
  def generateBootstrap(length: Int, executable: String) {
    val truncFile = preTruncFile + length.toString + postTruncFile

   (truncFile + "\n ${S2EGET} " + executable +
     "\n execute ./" + executable
     + "\n${S2ECMD} kill $? \"\'" + executable + "\' state killed\"")
  }

  val preTruncFile =
"""
#!/bin/bash
#
#

set -x

S2EGET=./s2eget
S2EPUT=./s2eput
S2ECMD=./s2ecmd
COMMON_TOOLS="s2ecmd s2eget s2eput"

# To save you the hassle of rebuilding the image every time you want to update
# S2E's guest tools, the first thing that we do is get the latest versions of
# the guest tools.
function update_guest_tools {
    local GUEST_TOOLS

    GUEST_TOOLS="$COMMON_TOOLS $(target_tools)"
    for TOOL in ${GUEST_TOOLS}; do
        ${S2EGET} guest-tools/${TOOL}
        chmod +x ${TOOL}
    done
}

function prepare_target {
    # Make sure that the target is executable
    chmod +x "$1"
}

# This prepares the symbolic file inputs.
# This function takes as input an optional seed file name.
# If the seed file is present, the commands makes the seed symbolic.
# Otherwise, it creates an empty file.
#
# Symbolic files must be stored in a ram disk, as only memory (and cpu)
# is capable of holding symbolic data.
#
# This function prints the path to the symbolic file on stdout.
function prepare_inputs {
    local SEED_FILE
    local SYMB_FILE

    # This can be empty if there are no seed files
    SEED_FILE="$1"
    SYMB_FILE="/tmp/input"


    if [ "x$SEED_FILE" = "x" ]; then
        # Create a symbolic file of size 256 bytes.
        # Note: you can customize this commands according to your needs.
        # You could, e.g., use non-zero input, different sizes, etc.

        truncate -s """

    val postTruncFile =
""" ${SYMB_FILE}


        if [ $? -ne 0 ]; then
            ${S2ECMD} kill 1 "Failed to create symbolic file"
            exit 1
        fi
    else
        cp ${SEED_FILE} ${SYMB_FILE}
    fi

    # Make thie file symbolic
    ${S2ECMD} symbfile ${SYMB_FILE}
    echo ${SYMB_FILE}
}

# This function executes the target program given in arguments.
#
# There are two versions of this function:
#    - without seed support
#    - with seed support (-s argument when creating projects with s2e_env)
function execute {
    local TARGET
    local SEED_FILE

    TARGET=$1

    prepare_target "${TARGET}"


    execute_target "${TARGET}"

}

###############################################################################
# This section contains target-specific code

# This function executes the target program.
# You can customize it if your program needs special invocation,
# custom symbolic arguments, etc.
function execute_target {
    local TARGET
    TARGET="$1"

    SYMB_FILE="$(prepare_inputs)"
    ./${TARGET} ${SYMB_FILE} > /dev/null 2> /dev/null

}



# Nothing more to initialize on Linux
function target_init {
    # Start the LinuxMonitor kernel module
    sudo modprobe s2e
}

# Returns Linux-specific tools
function target_tools {
    echo "s2e.so"
}

###############################################################################


update_guest_tools



# Don't print crashes in the syslog. This prevents unnecessary forking in the
# kernel
sudo sysctl -w debug.exception-trace=0

# Prevent core dumps from being created. This prevents unnecessary forking in
# the kernel
ulimit -c 0

# Ensure that /tmp is mounted in memory (if you built the image using s2e-env
# then this should already be the case. But better to be safe than sorry!)
if ! mount | grep "/tmp type tmpfs"; then
    sudo mount -t tmpfs -osize=10m tmpfs /tmp
fi



target_init

"""
}
