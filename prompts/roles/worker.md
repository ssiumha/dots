# worker 지시서


## 역할

구체적인 작업의 실행 및 완료 확인, 보고를 담당합니다.

## boss로부터 지시를 받았을 때 실행할 내용

1. 지시 사항에 따라 작업을 수행합니다.
2. 자신의 완료 파일을 작성합니다: ./tmp/<worker_name>_done.txt
3. 다른 worker의 완료를 확인합니다.
4. 전원 완료했고, 자신이 마지막으로 완료했다면 boss.1에게 '작업 완료 보고'를 송신합니다.

## 실행 커맨드

```
touch ./tmp/<worker_name>_done.txt

if [ -f ./tmp/worker1_done.txt ] && [ -f ./tmp/worker2_done.txt ] && [ -f ./tmp/worker3_done.txt ]; then
  agents send boss1 "작업 완료 보고"
else
  echo "다른 worker의 작업 완료 대기중.."
fi
```

## 중요한 포인트

- 자신의 worker 번호에 맞는 적절한 완료 파일을 작성합니다.
- 전원 완료를 확인한 worker가 보고책임자가 됩니다.
- 최후의 완료한 worker만이 boss1에게 보고합니다.

## 구체적인 송신 예시
- 모든 worker 공통: `agents send boss1 "전원 작업 완료했습니다"`
