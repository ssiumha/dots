module Lib
  class AwsChiz < Base
    md :pricing, 'ec2, s3, monthly pricing', <<~MD
      - ref: https://calculator.aws/
      - 보통 30일을 730.hours 으로 계산
      - 2302 기준

      73 USD    | EKS cluster
      5 USD     | t2.nano (1CPU, .5MEM)
      10 USD    | t2.micro (1CPU, 1MEM)
      81 USD    | m5.large (2CPU, 8MEM)

      0.03 USD  | S3 1GB
      25 USD    | S3 1TB
      5  USD    | EBS 4GB
      5.5 USD   | EBS 8GB
      6.5 USD   | EBS 16GB
      10  USD   | EBS gp2 32GB
      46  USD   | EBS gp2 256GB
      88  USD   | EBS gp2 512GB
      100 USD   | ECR 1TB

      450 USD   | RDS MySQL db.r4.large (30GB)
      379 USD   | RDS MySQL db.m5.large (30GB)
      186 USD   | RDS MySQL db.t2.medium (30GB)
    MD

    md :eks, 'kubeconfig update', <<~MD, lang: :sh
      aws eks update-kubeconfig --name $CLUSTER_NAME
    MD

    md :eksctl, 'eksctl cheatsheet', <<~MD, lang: :sh
      eksctl get iamserviceaccount --cluster $CLUSTER_NAME
    MD

    md :eks_addons, 'eks addon versions', <<~MD, lang: :sh
      aws eks describe-addon-versions --addon-name aws-ebs-csi-driver
    MD

    md :iam, 'iam cheatsheet', <<~MD, lang: :sh
      # IAM Policy simulator
      # https://policysim.aws.amazon.com/home/index.jsp?

      # User groups -> 공통 권한을 묶어놓는 용도. arn:aws:iam::xxxx:group/DevOps 형태
      # Users -> AWS 계정에 연결되거나 그 자체로 하나의 권한 단위. arn:aws:iam::xxxx:user/DevHuman 형태
      #     한 유저는 0개 이상의 그룹을 가질 수 있다
      # Roles -> 여러 policy의 집합. role 자체를 객체에 부여할 때 사용한다.  arn:aws:iam::xxxx:role/LogBucket 형태
      #     user와 다르게 고정된 주체가 없고, 임시로 여러 객체, 사용자에 할당되며 사용된다.
      # Policies -> permission 정의 단위. 만들어두면 Group, User에 설정시켜놓을 수 있다. arn:aws:iam::xxxx:policy/AllDB 형태
      #     어떤 IAM Prncipal이 어떤 Condition에서 AWS의 어떤 Resource에 대해 어떤 Action을 허용, 차단할지 지정한다
      #     Condition을 사용해서 특정 tag에 맞는 policy 컨트롤도 가능하다 (Condition.StringEquals.ec2:ResourceTag/Project: Blue)

      # 정책 검색
      aws iam list-policies --query 'Policies[?PolicyName==`AmazonS3ReadOnlyAccess`].Arn'

      # STS
      # https://docs.aws.amazon.com/en_us/IAM/latest/UserGuide/id_credentials_temp.html
      #  https://sts.amazonaws.com
      #  임시 보안 자격 증명을 만들 때 사용되는 서비스
    MD

    md :iam_policy, 'how to write policy', <<~MD
      # S3 Full Open
      ```
      {
        "Statement": [
          {
            "Action": [
              "s3:*",
              "s3-object-lambda:*"
            ],
            "Effect": "Allow",
            "Resource": "*"
          }
        ],
        "Version": "2012-10-17"
      }
      ```

      # S3 Open
      ```
      {
        "Statement": [
          {
            "Action": [
              "s3:ListBucket",
              "s3:PutObject",
              "s3:GetObject",
              "s3:DeleteObject"
            ],
            "Effect": "Allow",
            "Resource": [
              "arn:aws:s3:::fsl-dev-loki",
              "arn:aws:s3:::fsl-dev-loki/*"
            ]
          }
        ],
        "Version": "2012-10-17"
      }
      ```

      # Trust relationships
      ```
      {
          "Version": "2012-10-17",
          "Statement": [
              {
                  "Sid": "",
                  "Effect": "Allow",
                  "Principal": {
                      "Federated": "arn:aws:iam::000000000000:oidc-provider/oidc.eks.ap-northeast-2.amazonaws.com/id/00000000000000000000000000000000"
                  },
                  "Action": "sts:AssumeRoleWithWebIdentity",
                  "Condition": {
                      "StringEquals": {
                          "oidc.eks.ap-northeast-2.amazonaws.com/id/00000000000000000000000000000000:sub": "system:serviceaccount:kube-namespace:test-account"
                      }
                  }
              }
          ]
      }
      ```

      대충 OIDC 설정하며 나오는 설정. 설정에 따라 이 role의 위임을 시켜줄지 말지를 지정한다
    MD

    md :credentials, 'profile', <<~MD
      Role 설정할 때 Principal + sts:AssumeRole 설정으로 role 부여를 따로 관리할 수 있다
      실제 profile로는다음과 같이 쓰면 된다

      ```
      # ~/.aws/config
      [profile profileA]
      region = ap-northeast-1
      output = json

      [profile profileA-eks-operator]
      source_profile = profileA
      role_arn = arn:aws:iam::000000000000:role/eks-operator
      mfa_serial = arn:aws:iam::000000000000:mfa/A
      duration_seconds = 43200

      # kubeconfig에 profile을 넣어서 사용
      #   aws eks update-kubeconfig --name [dev-cluster] --profile profileA-eks-operator
      ```

      ```
      # ~/.aws/credentials
      [default]
      aws_access_key_id=XXXX
      aws_secret_access_key_id=XXXX

      [foo]
      ...
      ```
    MD
  end
end
