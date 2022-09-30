module Lib
  class TerraformChiz < Base
    md :flow, 'basic flow', <<~MD, lang: :sh
      terraform init

      terraform refresh

      # update tf files...

      terraform plan -out plan.out | tee plan.std.out
      terraform apply plan.out
    MD

    md :module, '', <<~MD
      ```tf
      module "awesome_module_name" {
        source = "terraform-aws-modules/eks/aws"
        version = "18.29.1"

        ...
      }
      ```

      여러 resource를 한번에 생성할 수록 tempalte화하여 묶은 것.
      source는 local, github, terraform registry, s3 bucket 등등으로 설정할 수 있다.

      사용된 값은 `module.awesome_module_name.cluster_id` 형태로 접근할 수 있다.
    MD

    md :resource, '', <<~MD
      ```tf
      resource "<resource_type>" "awesome_resource_name" {
      }
      ```

      테라폼에서 정의되는 자원의 최소단위
      특정 타입에 이름을 붙여 이름에 따른 서로 다른 설정을 지정해놓을 수 있다.

      `<resource_type>.awesome_resource_name` 형태로 다른 위치에서 불러들일 수 있다.
    MD

    md :data, '', <<~MD
      ```
      data "aws_subnets" "awesome_private_subnets" {
        for_each = toset(local.availability_zone_names)

        filter {
          name = "availabilityZone"
          values = [each.key]
        }
      }
      ```

      입력값을 통해 terraform에서 만들어지지 않은 정보를 취득, 가공하기 위한 기능.
      보통 resource output만으로 충분하지 않고 infra에 실제 올라간 일부 값이 필요할 때 사용된다.
    MD

    md :file_struct, '', <<~MD
      ```
      - main.tf
      - outputs.tf
      - variables.tf
      ```
    MD

    md :etc, 'locals, variable, outputs', <<~MD
      ```
      locals {
        tag_prefix = "dev"
      }
      ```
      - module 내에서 사용할 변수 값
      - `local.<NAME>` 으로 접근


      ```
      variable "env" {
        type = string
        default = "dev"
      }
      ```
      - 입력값으로 내부 동작을 조정할 때 사용
      - 보통 module로써 동작시킬 때 사용한다
      - `var.<NAME>` 으로 접근
    MD
  end
end
