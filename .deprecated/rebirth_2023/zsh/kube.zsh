alias -g kA='$( kubectl get all -A | fzf | perl -lane "print qq(-n @F[0] @F[1])" )'

alias k="kubectl"

alias kw="watch -n1 kubectl"
alias kwg="watch -n1 kubectl get"
alias ky="kubectl get -oyaml"
alias kg="kubectl get"
alias kd="kubectl describe"
alias ke="kubectl explain"

# has confused usecase.. ex) kga svc ...
#alias kwga="watch -n1 kubectl get all"
#alias kwgp="watch -n1 kubectl get pods"
#alias kya="kubectl get -oyaml all"
#alias kga="kubectl get all"
#alias kgp="kubectl get pods"

kl() {
  local ns n;
  kubectl get pod,deploy -A | fzf --header-lines=1 | awk '{print $1"\t"$2}' | IFS=$'\t' read -r ns n;
  kubectl logs -f -n $ns $n $@
}

ksw() {
  kubectl config get-contexts \
    | perl -nale 'next if $.==1; s/^(.)\s+([^\s]+).+$/$1 $2/; print' \
    | fzf | cut -c3- | xargs -I% kubectl config use-context %
}

# kubectl get pods -o wide --sort-by="{.spec.nodeName}" -A
# kubectl get pods -o wide --watch -A
# kubectl get pods --all-namespaces -o wide --field-selector spec.nodeName=ip-0.0.0.0.ap-northeast-2.compute.internal
# kubectl get pods --sort-by="{.status.containerStatuses[:1].restartCount}"
# kubectl get pods --template '{{range .items}}{{if eq .spec.nodeName "ip-10-0-90-30.ec2.internal"}}{{.metadata.name}}{{"\n"}}{{end}}}{{end}}'
# kubectl get servicemonitor,podmonitor -A -o json | jq -r '.items[] | [ .kind, .metadata.namespace, .metadata.labels.release, .metadata.name ] | @tsv' | column -s"\t" -c4
# kubectl get servicemonitor,podmonitor -A -o json | jq '[ .items[] | select( .metadata.labels.release | startswith("prometheus-operator") ) | [ .kind, .metadata.name, .metadata.namespace, .metadata.labels.release ] ]'
