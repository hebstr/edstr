jvm <- system("find /usr/lib -name libjvm.so 2>/dev/null", intern = TRUE)
if (length(jvm) > 0 && nzchar(jvm[1])) dyn.load(jvm[1])
