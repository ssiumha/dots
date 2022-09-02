module Lib
  class KubeChiz < Base
    md 'openapi', 'get kube all api list', <<~MD, lang: :bash
      kubectl get --raw /openapi/v2 | jq
    MD

    md 'export yaml', 'save yaml', <<~MD, lang: :bash
      kubectl get -nkube-system configmap aws-auth -o yaml > aws-auth.yaml
    MD
  end
end
