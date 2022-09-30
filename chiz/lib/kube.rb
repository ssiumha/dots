module Lib
  class KubeChiz < Base
    md :openapi, 'get kube all api list', <<~MD, lang: :bash
      kubectl get --raw /openapi/v2 | jq
    MD

    md :export_yaml, 'save yaml', <<~MD, lang: :bash
      kubectl get -nkube-system configmap aws-auth -o yaml > aws-auth.yaml
    MD

    md :helm, 'cheatsheet helm commands', <<~MD, lang: :bash
      # full diff
      helm diff upgrade $HELM_NAME $HELM_CHART \
        --namespace $KUBE_NAME_SPACE --values=values.yaml --allow-unreleased --debug $HELM_UPGRADE_ARGS
    MD

    md :prometheus, 'cheatsheet prometheus query', <<~MD
      PVC 사용 용량 가져오기 (GiB)
      ```promql
        (
          sum without(instance, node) (topk(1, (kubelet_volume_stats_capacity_bytes{job="kubelet", metrics_path="/metrics"})) by (persistentvolumeclaim))
          -
          sum without(instance, node) (topk(1, (kubelet_volume_stats_available_bytes{job="kubelet", metrics_path="/metrics"})) by (persistentvolumeclaim))
        ) / (1024 * 1024 * 1024)
      ```
    MD

    md :authorization, 'rbac, irsa, odic..', <<~MD
      RBAC : Role-based access control
        - 순수 kube 기능
        - Role, ClusterRole, RoleBinding, ClusterRoleBinding 리소스로 관리 된다
        - resource api에 대한 권한을 관리
          - resource는 pods, nodes, configmaps 등등
          - 권한은 get, watch, list, create, patch, update, delete, del-list API 동작

      OIDC : Open ID Connect
        - 그냥 프로토콜
        - OAuth의 유저 인증만 있던 것에 허가 기능을 추가하여 확장한 것
        - AccessToken에 더해 유저 정보가 포함된 ID Token도 취득한다
          - ID Token은 JWT 포맷

      IRSA : IAM Roles for Service Account
        - AWS의 IAM 권한을 Pod 별로 적용하기 위해 Service Account 기능으로 IAM Role 컨트롤을 구현한 것
        - kube2iam, kiam 등의 오픈소스를 써도 되지만 이들은 node 단위 기능
    MD
  end
end
