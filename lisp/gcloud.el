(add-to-list 'tramp-methods
             '("gcloud"
               (tramp-login-program        "gcloud compute ssh")
               (tramp-login-args           (("%h")))
               (tramp-async-args           (("-q")))
               (tramp-remote-shell         "/bin/sh")
               (tramp-remote-shell-args    ("-c"))
               (tramp-gw-args              (("-o" "GlobalKnownHostsFile=/dev/null")
                                            ("-o" "UserKnownHostsFile=/dev/null")
                                            ("-o" "StrictHostKeyChecking=no")))
               (tramp-default-port 22)))

(provide 'gcloud)
