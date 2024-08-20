#!/usr/bin/env bash

# Nodegroup과 kubectl에는 등록되어있지만,
# ASG에는 등록되어있지 않은 인스턴스를 찾는 스크립트
#
# Nodegroup의 Desired Capacity를 줄이면
# ASG에서 인스턴스가 잘 제거되었기 때문에 하단의 terminate는 주석처리

printf "CLUSTER_NAME: ";
CLUSTER_NAME=$(ruby -e 'puts gets.chomp');

printf "AWS_PROFILE: ";
AWS_PROFILE=$(ruby -e 'puts gets.chomp');

# all instance list (ASG base)
aws eks list-nodegroups --cluster-name ${CLUSTER_NAME} --output json | jq -r '.nodegroups[]' \
  | xargs -I% aws eks describe-nodegroup --cluster-name ${CLUSTER_NAME} --nodegroup % --query 'nodegroup.resources.autoScalingGroups[*].name' --output text \
  | xargs -I% aws autoscaling describe-auto-scaling-groups --auto-scaling-group-names % --query 'AutoScalingGroups[*].Instances[*].InstanceId' --output json \
  | jq -r '.[][]' | sort > all_asg_instance.txt

# kubectl node instance list
kubectl get nodes -o custom-columns='NAME:.metadata.name,INSTANCE:.metadata.annotations.csi\.volume\.kubernetes\.io/nodeid' \
  | awk -F":" '{ print $2 }' | perl -pe 's/"(i-.+?)".+/\1/' | sort > has_real.txt

comm -23 all_asg_instance.txt has_real.txt > target.txt

# terminate
#for line in $(cat target.txt); do
#  aws ec2 describe-instances --instance-ids $line --query 'Reservations[*].Instances[*].[InstanceId, State.Name, Tags[*].Value]' --output json;

#  #yn=$(ruby -e 'puts gets.chomp');
#  printf "Terminate? (y/n): ";
#  yn=$(perl -e 'chomp($i=<STDIN>);print $i');

#  if [ "$yn" = "y" ]; then
#    aws ec2 terminate-instances --instance-ids $line | tee log.txt
#  else
#    echo "Skip";
#  fi
#done
