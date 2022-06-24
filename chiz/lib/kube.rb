module Lib
  class KubeChiz < Base
    md 'openapi', 'get kube all api list', <<~MD, lang: :bash
      kubectl get --raw /openapi/v2 | jq
    MD
  end
end
