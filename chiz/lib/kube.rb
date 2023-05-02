module Lib
  class KubeChiz < Base
    md :reference, 'document', <<~MD
      - https://kubernetes.io/docs/reference/generated/kubernetes-api/v1.25/#envvar-v1-core
    MD

    md :mysql_client, 'attach pod', <<~MD
      kubectl run -it --rm mysql-client --image=mysql:5.7 --restart=Never -- /bin/bash

      mysql -h xxxx.cluster.ap-northeast-2.rds.amazonaws.com -u root -p
    MD

    md :telepresence, 'connect, install', <<~MD, lang: :bash
      # 현재 클러스터에 ambassador/traffic-manager를 추가
      telepresence helm install

      # local에 daemon 띄움 + traffic-manager와 연결
      telepresence connect

      # local의 daemon 내림
      telepresence quit -s
    MD

    md :openapi, 'get kube all api list', <<~MD, lang: :bash
      kubectl get --raw /openapi/v2 | jq
    MD

    md :export_yaml, 'save yaml', <<~MD, lang: :bash
      kubectl get -nkube-system configmap aws-auth -o yaml > aws-auth.yaml
    MD

    md :cordon, 'drain', <<~MD, lang: :bash
      # drain으로 node 갈아치우기
      #   - EKS 환경에서 kubectl delete node를 쳐도 AGS쪽 인스턴스에는 남아서 scaleup/down에 영향을 끼칠 때가 있다
      #     이때는 직접 node를 terminate 시켜줘야한다
      # cordon으로 해당 node에 pod가 못뜨게 막은 다음, drain으로 pod를 전부 재할당하고 노드를 제거한다
      kubectl cordon <node/ip-00-00-00-00>
      kubectl drain --ignore-daemonsets --delete-emptydir-data <node/ip-00-00-00-00>

      kubectl delete <node/ip-00-00-00-00>

      # Cannot evict pod as it would violate the pod's disruption budget
      # 노드가 인위적으로 줄이면 (Voluntary disruptions) 총 Pod 수가 변경된다.
      # 이때 성능이나 안정성 때문에 쿼럼값 만큼 최소 Pod를 유지해야할 때 사용하는 기능이 PodDistruptionBudget이다.
      # Autoscaler로 인해 노드가 감소하더라도 일정 수준으로 Pod를 유지시켜준다
      # spec.minAvailable : 항상 이 숫자만큼은 유지되어야한다
      # spec.maxUnavailable : 최대 이 숫자만큼 종료될 수 있다. "50%"도 지정 가능
    MD

    md :helm, 'cheatsheet helm commands', <<~MD, lang: :bash
      # full diff
      helm diff upgrade $HELM_NAME $HELM_CHART \
        --namespace $KUBE_NAME_SPACE --values=values.yaml --allow-unreleased --debug $HELM_UPGRADE_ARGS
    MD

    md :output, 'get jsonpath, field', <<~MD, lang: :bash
      # ref: https://kubernetes.io/docs/reference/kubectl/jsonpath/
      kubectl get crd -A -o jsonpath="{range .items[*]}{ ['apiVersion', 'kind', 'metadata.name'] }{'\\n'}"
    MD

    md :resource, 'resource limit, request', <<~MD
      request
        - pod를 띄울 때, node에 띄울 수 있는지를 판단할 때 사용하는 값
        - pod는 이 값을 넘어서 메모리, CPU를 사용할 수 있다


      limits
        - pod의 실질적인 사용량 제한
        - CPU는 limit을 넘어서면 throttle이 발생
        - Memory는 limit을 넘어서면 OOM Kill이 발생한다

      CPU
        - CPU request는 Core 갯수에 해당한다
        - CPU limit은 cfs_period_us에 의거한 throttle 제약 시간이다

      CPU Throttle
        - CPU Limit을 구성할 떄, 잘못지정하면 Throttle은 발생하지 않지만 응답 시간이 지연될 수 있다
        - CPU Limit을 200ms 로 설정하면, cgroup 에서는 100ms 기준으로 설정되며 20ms가 지정된다
          - 따라서 응답 시간이 20ms가 넘어가면 처리가 4배는 느려진다

        - 리소스는 Linux의 cgroup을 통해 컨트롤된다 (1코어 = 1024 고정값)
          - root 4096을 쓸 수 있을 때, system, user에서 1024, kubepods가 4096을 가져가게 된다
          - 실제 공유 CPU의 합계는 6144로 원래 CPU 코어를 초과하지만, 리눅스 스케쥴러(CFS)는 이를 비율로 치환해서 할당한다
          - 1024 : 1024 : 4096 -> 16% : 16% : 68%. 따라서 kubepods는 2736ms 만큼을 얻는다
          - kube는 받은 CPU 처리를 위의 CPU 코어 기준이 아니라 cfs_period_us, 100ms의 기간을 기반으로 작동한다
            - --cpu-cfs-quota-period 옵션으로 조정은 할 수 있다

        - 스레드 2개 + 200ms limit -> throttling 없음
        - 스레드 10개 + 200ms limit
          - 20ms CPU 코어 사용 후, 80ms 동안 throttling이 발생한다
          - 10 * 20ms = 200ms
            - 20ms만에 할당된 200ms limit에 도달하며 100ms (cfs_period 기본 단위) - 20ms(사용한 cpu) = 80ms
            - 80ms 동안 throttle이 발생한다

      ref:
        - https://community.ibm.com/community/user/aiops/blogs/dina-henderson/2022/06/29/kubernetes-cpu-throttling-the-silent-killer-of-res
        - https://medium.com/omio-engineering/cpu-limits-and-aggressive-throttling-in-kubernetes-c5b20bd8a718
    MD

    md :prometheus, 'cheatsheet prometheus query', <<~MD
      # PVC 사용 용량 가져오기 (GiB)
      ```promql
        (
          sum without(instance, node) (topk(1, (kubelet_volume_stats_capacity_bytes{job="kubelet", metrics_path="/metrics"})) by (persistentvolumeclaim))
          -
          sum without(instance, node) (topk(1, (kubelet_volume_stats_available_bytes{job="kubelet", metrics_path="/metrics"})) by (persistentvolumeclaim))
        ) / (1024 * 1024 * 1024)
      ```

      # CPU 체크 (500m = 0.5cpu)
      ```promql
        # container usage
        rate(container_cpu_usage_seconds_total{}[5m])

        # container requests
        avg(kube_pod_container_resource_requests_cpu_cores{})

        # container limits
        avg(kube_pod_container_resource_limits_cpu_cores{})

        # throttling
        rate(container_cpu_cfs_throttled_seconds_total{}[5m])
      ```

      # Memory 체크
      ```promql
        sum by (namespace, pod, instance) (container_memory_usage_bytes{namespace="helm"})
        / sum by (namespace, pod, instance) (container_spec_memory_limit_bytes{namespace="helm"})
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
