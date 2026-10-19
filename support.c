#include <unistd.h>
extern char** environ;

#include <string.h>
#include <errno.h>
#include <assert.h>
#include <fcntl.h>

#include <sodium.h>

#define HASH_LEN 32

char * prim__sha256(void * msg, int len) {
    unsigned char out[crypto_hash_sha256_BYTES];
    crypto_hash_sha256(out, msg, len);
    char * ret = malloc(HASH_LEN + 1);
    char * x = "0123456789abcdef";
    for (int i = 0; i < HASH_LEN / 2; i++) {
        ret[i*2+1] = x[out[i] >> 4 & 0x0f];
        ret[i*2  ] = x[out[i]      & 0x0f];
    }

    ret[HASH_LEN] = 0;

    return ret;
}

static char ** split0(char * buf, int n) {
    char ** ret = malloc(n * sizeof(char *) + 1);
    ret[0] = buf;
    ret[n] = NULL;

    for (int i = 1; i < n; i++) {
        while (*buf++) { }
        ret[i] = buf;
    }

    /*
    for (int i = 0; i < n; i++) {
        printf("%s\n", ret[i]);
    }
    */

    return ret;
}


int prim__exec(
    char * envsv,
    int nenvs,
    char * argsv,
    int nargs,
    char * newout,
    int has_newout
) {
    assert(nargs > 0);

    char ** args = split0(argsv, nargs);
    char ** envs = split0(envsv, nenvs);

    printf("execing\n");

    printf("errno before: %d\n", errno);

    pid_t pid = fork();
    int ret = 0;
    if (pid == 0) {
        printf("errno child: %d\n", errno);

        printf("in child\n");
        printf("path:'%s'\n", args[0]);
        for (int i = 0; i < nargs; i++) {
            printf("arg %d: '%s'\n", i, args[i]);
        }

        printf("has_newout: %d\n", has_newout);
        printf("newout: '%s'\n", newout);

        if (has_newout) {
            printf("before open: %d\n", errno);
            int fd = open(newout, O_CREAT|O_TRUNC|O_WRONLY, 0600);
            if (fd == -1) {
                fprintf(stderr, "failed open: %s\n", strerror(errno));
                exit(errno);
            }

            fd = dup2(fd, STDOUT_FILENO);
            if (fd == -1) {
                fprintf(stderr, "failed dup2: %s\n", strerror(errno));
                exit(errno);
            }
        }

        environ = envs;

        int ret = execvp(args[0], args);
        if (errno) {
            fprintf(stderr, "failed exec: %s\n", strerror(errno));
            exit(errno);
        }

        assert(!"unreachable section reached");
    } else if (pid < 0) {
        printf("failed fork: %s\n", strerror(errno));
        ret = errno;
    } else {
        printf("errno parent: %d\n", errno);
        int stat = 0;
        waitpid(pid, &stat, 0);
        printf("stat: %d\n", WEXITSTATUS(stat));
        ret = WEXITSTATUS(stat);
    }

    free(args);
    free(envs);

    return ret;
}
