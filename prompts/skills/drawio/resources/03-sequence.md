# Sequence 다이어그램 패턴

## 개요

객체 간 상호작용, API 호출 흐름, 메시지 교환을 시간 순서로 표현하는 다이어그램.

## 기본 구성요소

| 요소 | 용도 | 스타일 |
|------|------|--------|
| 참여자 (Actor) | 사용자, 외부 시스템 | shape=actor |
| 객체 (Object) | 시스템, 서비스 | 기본 박스 |
| 생명선 (Lifeline) | 시간 흐름 | 점선 세로선 |
| 메시지 (Message) | 호출/응답 | 화살표 |
| 활성 박스 | 실행 중 표시 | 좁은 사각형 |

## 스타일 가이드

### 참여자 색상
```
사용자/액터:    fillColor=#E3F2FD (연한 파랑)
클라이언트:     fillColor=#E8F5E9 (연한 초록)
서버/서비스:    fillColor=#FFFFFF (흰색)
데이터베이스:   fillColor=#FFF3E0 (연한 주황)
외부 시스템:    fillColor=#F5F5F5 (연한 회색)
```

### 메시지 스타일
```
동기 호출:      strokeColor=#424242, endArrow=classic (실선)
비동기 호출:    strokeColor=#424242, endArrow=open (실선)
응답:          strokeColor=#757575, dashed=1, endArrow=classic (점선)
자기 호출:      curved=1, 루프 형태
```

## XML 템플릿

### 기본 시퀀스 다이어그램

```xml
<mxfile host="app.diagrams.net" compressed="false">
  <diagram name="Sequence" id="seq-1">
    <!-- defaultFontFamily: 한글 폰트 지정 (예: Noto Sans KR, Malgun Gothic) -->
    <mxGraphModel dx="1434" dy="836" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="827" pageHeight="1169">
      <root>
        <mxCell id="0" />
        <mxCell id="1" parent="0" />

        <!-- 생명선 및 메시지 (edge를 먼저 기술 - 최배면) -->
        <mxCell id="line1" value="" style="endArrow=none;dashed=1;html=1;strokeColor=#BDBDBD;" edge="1" parent="1">
          <mxGeometry relative="1" as="geometry">
            <mxPoint x="95" y="120" as="sourcePoint" />
            <mxPoint x="95" y="400" as="targetPoint" />
          </mxGeometry>
        </mxCell>
        <mxCell id="line2" value="" style="endArrow=none;dashed=1;html=1;strokeColor=#BDBDBD;" edge="1" parent="1">
          <mxGeometry relative="1" as="geometry">
            <mxPoint x="200" y="80" as="sourcePoint" />
            <mxPoint x="200" y="400" as="targetPoint" />
          </mxGeometry>
        </mxCell>
        <mxCell id="line3" value="" style="endArrow=none;dashed=1;html=1;strokeColor=#BDBDBD;" edge="1" parent="1">
          <mxGeometry relative="1" as="geometry">
            <mxPoint x="340" y="80" as="sourcePoint" />
            <mxPoint x="340" y="400" as="targetPoint" />
          </mxGeometry>
        </mxCell>
        <mxCell id="line4" value="" style="endArrow=none;dashed=1;html=1;strokeColor=#BDBDBD;" edge="1" parent="1">
          <mxGeometry relative="1" as="geometry">
            <mxPoint x="460" y="90" as="sourcePoint" />
            <mxPoint x="460" y="400" as="targetPoint" />
          </mxGeometry>
        </mxCell>

        <!-- 메시지들 -->
        <mxCell id="m1" value="1. login()" style="endArrow=classic;html=1;strokeColor=#424242;fontSize=18;" edge="1" parent="1">
          <mxGeometry relative="1" as="geometry">
            <mxPoint x="95" y="140" as="sourcePoint" />
            <mxPoint x="200" y="140" as="targetPoint" />
          </mxGeometry>
        </mxCell>
        <mxCell id="m2" value="2. POST /api/login" style="endArrow=classic;html=1;strokeColor=#424242;fontSize=18;" edge="1" parent="1">
          <mxGeometry relative="1" as="geometry">
            <mxPoint x="200" y="180" as="sourcePoint" />
            <mxPoint x="340" y="180" as="targetPoint" />
          </mxGeometry>
        </mxCell>
        <mxCell id="m3" value="3. SELECT user" style="endArrow=classic;html=1;strokeColor=#424242;fontSize=18;" edge="1" parent="1">
          <mxGeometry relative="1" as="geometry">
            <mxPoint x="340" y="220" as="sourcePoint" />
            <mxPoint x="460" y="220" as="targetPoint" />
          </mxGeometry>
        </mxCell>
        <mxCell id="m4" value="4. user data" style="endArrow=classic;html=1;strokeColor=#757575;dashed=1;fontSize=18;" edge="1" parent="1">
          <mxGeometry relative="1" as="geometry">
            <mxPoint x="460" y="260" as="sourcePoint" />
            <mxPoint x="340" y="260" as="targetPoint" />
          </mxGeometry>
        </mxCell>
        <mxCell id="m5" value="5. 200 OK + token" style="endArrow=classic;html=1;strokeColor=#757575;dashed=1;fontSize=18;" edge="1" parent="1">
          <mxGeometry relative="1" as="geometry">
            <mxPoint x="340" y="300" as="sourcePoint" />
            <mxPoint x="200" y="300" as="targetPoint" />
          </mxGeometry>
        </mxCell>
        <mxCell id="m6" value="6. success" style="endArrow=classic;html=1;strokeColor=#757575;dashed=1;fontSize=18;" edge="1" parent="1">
          <mxGeometry relative="1" as="geometry">
            <mxPoint x="200" y="340" as="sourcePoint" />
            <mxPoint x="95" y="340" as="targetPoint" />
          </mxGeometry>
        </mxCell>

        <!-- 참여자들 (vertex를 나중에 기술 - 전면) -->
        <mxCell id="actor" value="User" style="shape=umlActor;verticalLabelPosition=bottom;verticalAlign=top;html=1;fillColor=#E3F2FD;strokeColor=#1976D2;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="80" y="40" width="30" height="60" as="geometry" />
        </mxCell>
        <mxCell id="client" value="Client" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#E8F5E9;strokeColor=#388E3C;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="160" y="40" width="80" height="40" as="geometry" />
        </mxCell>
        <mxCell id="server" value="Server" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#FFFFFF;strokeColor=#424242;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="300" y="40" width="80" height="40" as="geometry" />
        </mxCell>
        <mxCell id="db" value="DB" style="shape=cylinder3;whiteSpace=wrap;html=1;fillColor=#FFF3E0;strokeColor=#F57C00;size=10;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="430" y="30" width="60" height="60" as="geometry" />
        </mxCell>
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>
```

## 메시지 유형

### 동기 호출 (실선 + 꽉찬 화살표)

```xml
<mxCell id="sync" value="request()" style="endArrow=classic;html=1;strokeColor=#424242;" edge="1" parent="1">
  <mxGeometry relative="1" as="geometry">
    <mxPoint x="100" y="150" as="sourcePoint" />
    <mxPoint x="250" y="150" as="targetPoint" />
  </mxGeometry>
</mxCell>
```

### 비동기 호출 (실선 + 열린 화살표)

```xml
<mxCell id="async" value="emit(event)" style="endArrow=open;html=1;strokeColor=#424242;endFill=0;" edge="1" parent="1">
  <mxGeometry relative="1" as="geometry">
    <mxPoint x="100" y="150" as="sourcePoint" />
    <mxPoint x="250" y="150" as="targetPoint" />
  </mxGeometry>
</mxCell>
```

### 응답 (점선 + 화살표)

```xml
<mxCell id="response" value="return data" style="endArrow=classic;html=1;strokeColor=#757575;dashed=1;" edge="1" parent="1">
  <mxGeometry relative="1" as="geometry">
    <mxPoint x="250" y="200" as="sourcePoint" />
    <mxPoint x="100" y="200" as="targetPoint" />
  </mxGeometry>
</mxCell>
```

### 자기 호출 (Self-call)

```xml
<mxCell id="self" value="validate()" style="endArrow=classic;html=1;strokeColor=#424242;curved=1;" edge="1" parent="1">
  <mxGeometry relative="1" as="geometry">
    <mxPoint x="200" y="180" as="sourcePoint" />
    <mxPoint x="200" y="220" as="targetPoint" />
    <Array as="points">
      <mxPoint x="250" y="180" />
      <mxPoint x="250" y="220" />
    </Array>
  </mxGeometry>
</mxCell>
```

## 활성 박스 (Activation Box)

```xml
<!-- 활성 박스 -->
<mxCell id="activation" value="" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#E3F2FD;strokeColor=#1976D2;" vertex="1" parent="1">
  <mxGeometry x="195" y="180" width="10" height="80" as="geometry" />
</mxCell>
```

## 레이아웃 팁

1. **참여자 배치**: 왼쪽부터 호출 순서대로 배치
2. **생명선 간격**: 참여자 간 140-160px
3. **메시지 간격**: 메시지 간 40-60px
4. **번호 매기기**: 메시지에 순서 번호 표시 (1, 2, 3...)
5. **그룹화**: 관련 메시지는 박스로 묶기 (opt, alt, loop)
6. **수직 정렬**: 시간은 위에서 아래로 흐름
