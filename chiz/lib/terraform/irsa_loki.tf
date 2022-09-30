# OIDC 설정하고 한번에 IRSA ROLE 설정해버리기
# - IAM ROLE이 만들어졌다면, Condition이랑 일치하는 service account에 annotations를 추가하면 된다
#   annotations:
#   - eks.amazonaws.com/role-arn: arn:aws:iam::000000000000:role/irsa-loki

# 필요권한
#   iam:CreateOpenIdConnectProvider
#   iam:CreatePolicyVersion
#   iam:DeletePolicyVersion
#   iam:PutRolePolicy

data "tls_certificate" "eks" {
  url = module.eks.cluster_oidc_issuer_url
}

resource "aws_iam_openid_connect_provider" "eks" {
  client_id_list = ["sts.amazonaws.com"]
  thumbprint_list = [data.tls_certificate.eks.certificates[0].sha1_fingerprint]
  url = module.eks.cluster_oidc_issuer_url
}

resource "aws_iam_role" "irsa-loki" {
  name = "irsa-loki"

  assume_role_policy = jsonencode({
    Version = "2012-10-17"
    Statement = [
      {
        Sid = ""
        Effect = "Allow"
        Action = "sts:AssumeRoleWithWebIdentity"

        Principal = {
          Federated = aws_iam_openid_connect_provider.eks.arn
        }

        Condition = {
          StringEquals = {
            "${replace(aws_iam_openid_connect_provider.eks.url, "https://", "")}:sub" = "system:serviceaccount:kube-service:kube-sa"
          }
        }
      }
    ]
  })

  inline_policy {
    name = "loki-s3-policy"
    policy = jsonencode({
      Version = "2012-10-17"
      Statement = [{
        Effect = "Allow"
        Action = [
          "s3:ListBucket",
          "s3:PutObject",
          "s3:GetObject",
          "s3:DeleteObject"
        ]
        Resource = [
          "arn:aws:s3:::loki",
          "arn:aws:s3:::loki/*"
        ]
      }]
    })
  }
}
