p: sys_exit
  (opt-flush)
  regalloc-reset
  free-eax
  1  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_fork
  (opt-flush)
  regalloc-reset
  free-eax
  2  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_read
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  3  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_write
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  4  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_open
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  5  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_close
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  6  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_waitpid
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  7  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_creat
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  8  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_link
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  9  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_unlink
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  10  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_execve
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  11  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_chdir
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  12  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_time
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  13  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_mknod
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  14  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_chmod
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  15  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_chown
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  16  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_lseek
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  19  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_getpid
  (opt-flush)
  regalloc-reset
  free-eax
  20  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_mount
  (opt-flush)
  regalloc-reset
  req-edi
  req-esi
  req-edx
  req-ecx
  req-ebx
  free-eax
  21  ## eax mov,
  $$ 80 int,
  5 reg-free
  0 free>tos
  ;

p: sys_umount
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  22  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_setuid
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  23  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_getuid
  (opt-flush)
  regalloc-reset
  free-eax
  24  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_stime
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  25  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_ptrace
  (opt-flush)
  regalloc-reset
  req-esi
  req-edx
  req-ecx
  req-ebx
  free-eax
  26  ## eax mov,
  $$ 80 int,
  4 reg-free
  0 free>tos
  ;

p: sys_alarm
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  27  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_pause
  (opt-flush)
  regalloc-reset
  free-eax
  29  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_utime
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  30  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_access
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  33  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_nice
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  34  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_ftime
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  35  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_sync
  (opt-flush)
  regalloc-reset
  free-eax
  36  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_kill
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  37  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_rename
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  38  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_mkdir
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  39  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_rmdir
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  40  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_dup
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  41  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_pipe
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  42  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_times
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  43  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_brk
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  45  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_setgid
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  46  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_getgid
  (opt-flush)
  regalloc-reset
  free-eax
  47  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_signal
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  48  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_geteuid
  (opt-flush)
  regalloc-reset
  free-eax
  49  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_getegid
  (opt-flush)
  regalloc-reset
  free-eax
  50  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_acct
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  51  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_phys
  (opt-flush)
  regalloc-reset
  req-esi
  req-edx
  req-ecx
  req-ebx
  free-eax
  52  ## eax mov,
  $$ 80 int,
  4 reg-free
  0 free>tos
  ;

p: sys_ioctl
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  54  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_fcntl
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  55  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_setpgid
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  57  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_umask
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  60  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_chroot
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  61  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_ustat
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  62  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_dup2
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  63  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_getppid
  (opt-flush)
  regalloc-reset
  free-eax
  64  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_getpgrp
  (opt-flush)
  regalloc-reset
  free-eax
  65  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_setsid
  (opt-flush)
  regalloc-reset
  free-eax
  66  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_sigaction
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  67  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_setreuid
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  70  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_setregid
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  71  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_sigsuspend
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  72  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_sigpending
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  73  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_sethostname
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  74  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_setrlimit
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  75  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_getrlimit
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  76  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_getrusage
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  77  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_gettimeofday
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  78  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_settimeofday
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  79  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_getgroups
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  80  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_setgroups
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  81  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_select
  (opt-flush)
  regalloc-reset
  req-edi
  req-esi
  req-edx
  req-ecx
  req-ebx
  free-eax
  82  ## eax mov,
  $$ 80 int,
  5 reg-free
  0 free>tos
  ;

p: sys_symlink
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  83  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_readlink
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  85  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_uselib
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  86  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_swapon
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  87  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_reboot
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  88  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_readdir
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  89  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_munmap
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  91  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_truncate
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  92  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_ftruncate
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  93  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_fchmod
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  94  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_fchown
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  95  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_getpriority
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  96  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_setpriority
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  97  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_profil
  (opt-flush)
  regalloc-reset
  req-esi
  req-edx
  req-ecx
  req-ebx
  free-eax
  98  ## eax mov,
  $$ 80 int,
  4 reg-free
  0 free>tos
  ;

p: sys_statfs
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  99  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_fstatfs
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  100  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_ioperm
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  101  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_socketcall
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  102  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_syslog
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  103  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_setitimer
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  104  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_getitimer
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  105  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_stat
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  106  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_lstat
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  107  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_fstat
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  108  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_iopl
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  110  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_vhangup
  (opt-flush)
  regalloc-reset
  free-eax
  111  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_idle
  (opt-flush)
  regalloc-reset
  free-eax
  112  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_vm86
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  113  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_wait4
  (opt-flush)
  regalloc-reset
  req-esi
  req-edx
  req-ecx
  req-ebx
  free-eax
  114  ## eax mov,
  $$ 80 int,
  4 reg-free
  0 free>tos
  ;

p: sys_swapoff
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  115  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_sysinfo
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  116  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_fsync
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  118  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_sigreturn
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  119  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_clone
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  120  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_setdomainname
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  121  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_uname
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  122  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_modify_ldt
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  123  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_adjtimex
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  124  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_mprotect
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  125  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_sigprocmask
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  126  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_create_module
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  127  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_init_module
  (opt-flush)
  regalloc-reset
  req-edi
  req-esi
  req-edx
  req-ecx
  req-ebx
  free-eax
  128  ## eax mov,
  $$ 80 int,
  5 reg-free
  0 free>tos
  ;

p: sys_delete_module
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  129  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_get_kernel_syms
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  130  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_quotactl
  (opt-flush)
  regalloc-reset
  req-esi
  req-edx
  req-ecx
  req-ebx
  free-eax
  131  ## eax mov,
  $$ 80 int,
  4 reg-free
  0 free>tos
  ;

p: sys_getpgid
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  132  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_fchdir
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  133  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_bdflush
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  134  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_sysfs
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  135  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_personality
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  136  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_setfsuid
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  138  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_setfsgid
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  139  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_llseek
  (opt-flush)
  regalloc-reset
  req-edi
  req-esi
  req-edx
  req-ecx
  req-ebx
  free-eax
  140  ## eax mov,
  $$ 80 int,
  5 reg-free
  0 free>tos
  ;

p: sys_getdents
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  141  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_flock
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  143  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_msync
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  144  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_readv
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  145  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_writev
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  146  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_getsid
  (opt-flush)
  regalloc-reset
  free-eax
  147  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_fdatasync
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  148  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_sysctl
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  149  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_mlock
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  150  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_munlock
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  151  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_mlockall
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  152  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_munlockall
  (opt-flush)
  regalloc-reset
  free-eax
  153  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_sched_setparam
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  154  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_sched_getparam
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  155  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_sched_setscheduler
  (opt-flush)
  regalloc-reset
  req-edx
  req-ecx
  req-ebx
  free-eax
  156  ## eax mov,
  $$ 80 int,
  3 reg-free
  0 free>tos
  ;

p: sys_sched_getscheduler
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  157  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_sched_yield
  (opt-flush)
  regalloc-reset
  free-eax
  158  ## eax mov,
  $$ 80 int,
  0 reg-free
  0 free>tos
  ;

p: sys_sched_get_priority_max
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  159  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_sched_get_priority_min
  (opt-flush)
  regalloc-reset
  req-ebx
  free-eax
  160  ## eax mov,
  $$ 80 int,
  1 reg-free
  0 free>tos
  ;

p: sys_sched_rr_get_interval
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  161  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_nanosleep
  (opt-flush)
  regalloc-reset
  req-ecx
  req-ebx
  free-eax
  162  ## eax mov,
  $$ 80 int,
  2 reg-free
  0 free>tos
  ;

p: sys_mremap
  (opt-flush)
  regalloc-reset
  req-esi
  req-edx
  req-ecx
  req-ebx
  free-eax
  163  ## eax mov,
  $$ 80 int,
  4 reg-free
  0 free>tos
  ;

