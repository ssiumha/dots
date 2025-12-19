# Flowchart 다이어그램 패턴

## 개요

프로세스 흐름, 의사결정 분기, 워크플로우를 표현하는 다이어그램.

## 기본 도형

| 도형 | 용도 | shape 스타일 |
|------|------|-------------|
| 타원 | 시작/종료 | `ellipse` |
| 사각형 | 프로세스/작업 | (기본) |
| 마름모 | 의사결정 | `rhombus` |
| 평행사변형 | 입력/출력 | `parallelogram` |
| 실린더 | 데이터 저장 | `cylinder3` |

## 스타일 가이드

### 도형별 색상
```
시작/종료:   fillColor=#E8F5E9 (연한 초록)
프로세스:    fillColor=#FFFFFF (흰색)
의사결정:    fillColor=#FFF3E0 (연한 주황)
입력/출력:   fillColor=#E3F2FD (연한 파랑)
에러/예외:   fillColor=#FFEBEE (연한 빨강)
```

### 연결선
```
정상 흐름:   strokeColor=#424242 (검정)
Yes/True:   strokeColor=#388E3C (초록), value="Yes"
No/False:   strokeColor=#D32F2F (빨강), value="No"
```

## XML 템플릿

### 기본 플로우차트

```xml
<mxfile host="app.diagrams.net" compressed="false">
  <diagram name="Flowchart" id="flow-1">
    <!-- defaultFontFamily: 한글 폰트 지정 (예: Noto Sans KR, Malgun Gothic) -->
    <mxGraphModel dx="1434" dy="836" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="827" pageHeight="1169">
      <root>
        <mxCell id="0" />
        <mxCell id="1" parent="0" />

        <!-- 연결선들 (edge를 먼저 기술 - 최배면) -->
        <mxCell id="e1" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;" edge="1" parent="1" source="2" target="3">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="e2" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;" edge="1" parent="1" source="3" target="4">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="e3" value="Yes" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;strokeColor=#388E3C;fontColor=#388E3C;fontSize=18;" edge="1" parent="1" source="4" target="5">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="e4" value="No" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;strokeColor=#D32F2F;fontColor=#D32F2F;fontSize=18;" edge="1" parent="1" source="4" target="6">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="e5" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;" edge="1" parent="1" source="5" target="7">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="e6" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;exitX=0.5;exitY=1;entryX=1;entryY=0.5;" edge="1" parent="1" source="6" target="3">
          <mxGeometry relative="1" as="geometry">
            <Array as="points">
              <mxPoint x="370" y="160" />
            </Array>
          </mxGeometry>
        </mxCell>

        <!-- 시작 -->
        <mxCell id="2" value="시작" style="ellipse;whiteSpace=wrap;html=1;fillColor=#E8F5E9;strokeColor=#388E3C;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="160" y="40" width="80" height="50" as="geometry" />
        </mxCell>

        <!-- 프로세스 1 -->
        <mxCell id="3" value="데이터 입력" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#FFFFFF;strokeColor=#424242;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="140" y="130" width="120" height="60" as="geometry" />
        </mxCell>

        <!-- 의사결정 -->
        <mxCell id="4" value="유효한가?" style="rhombus;whiteSpace=wrap;html=1;fillColor=#FFF3E0;strokeColor=#F57C00;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="140" y="230" width="120" height="80" as="geometry" />
        </mxCell>

        <!-- 프로세스 2 (Yes) -->
        <mxCell id="5" value="처리 실행" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#FFFFFF;strokeColor=#424242;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="140" y="360" width="120" height="60" as="geometry" />
        </mxCell>

        <!-- 에러 처리 (No) -->
        <mxCell id="6" value="에러 처리" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#FFEBEE;strokeColor=#D32F2F;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="320" y="250" width="120" height="40" as="geometry" />
        </mxCell>

        <!-- 종료 -->
        <mxCell id="7" value="종료" style="ellipse;whiteSpace=wrap;html=1;fillColor=#E8F5E9;strokeColor=#388E3C;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="160" y="470" width="80" height="50" as="geometry" />
        </mxCell>
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>
```

## 분기 패턴

### 단일 분기 (Yes/No)

```xml
<!-- 의사결정 -->
<mxCell id="dec" value="조건?" style="rhombus;whiteSpace=wrap;html=1;fillColor=#FFF3E0;strokeColor=#F57C00;" vertex="1" parent="1">
  <mxGeometry x="140" y="200" width="100" height="80" as="geometry" />
</mxCell>

<!-- Yes 경로 (아래) -->
<mxCell id="yes" value="Yes" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;strokeColor=#388E3C;fontColor=#388E3C;" edge="1" parent="1" source="dec" target="next">
  <mxGeometry relative="1" as="geometry" />
</mxCell>

<!-- No 경로 (오른쪽) -->
<mxCell id="no" value="No" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;strokeColor=#D32F2F;fontColor=#D32F2F;exitX=1;exitY=0.5;" edge="1" parent="1" source="dec" target="alt">
  <mxGeometry relative="1" as="geometry" />
</mxCell>
```

### 다중 분기 (Switch)

```xml
<!-- 의사결정 -->
<mxCell id="switch" value="타입?" style="rhombus;whiteSpace=wrap;html=1;fillColor=#FFF3E0;strokeColor=#F57C00;" vertex="1" parent="1">
  <mxGeometry x="200" y="100" width="100" height="80" as="geometry" />
</mxCell>

<!-- 분기 1 -->
<mxCell id="b1" value="A" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;exitX=0;exitY=0.5;" edge="1" parent="1" source="switch" target="optA">
  <mxGeometry relative="1" as="geometry" />
</mxCell>

<!-- 분기 2 -->
<mxCell id="b2" value="B" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;exitX=0.5;exitY=1;" edge="1" parent="1" source="switch" target="optB">
  <mxGeometry relative="1" as="geometry" />
</mxCell>

<!-- 분기 3 -->
<mxCell id="b3" value="C" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;exitX=1;exitY=0.5;" edge="1" parent="1" source="switch" target="optC">
  <mxGeometry relative="1" as="geometry" />
</mxCell>
```

## 반복 패턴

### 루프백 (재시도)

```xml
<!-- 루프백 연결 -->
<mxCell id="loop" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;dashed=1;endArrow=classic;exitX=1;exitY=0.5;entryX=1;entryY=0.5;" edge="1" parent="1" source="process" target="check">
  <mxGeometry relative="1" as="geometry">
    <Array as="points">
      <mxPoint x="350" y="270" />
      <mxPoint x="350" y="160" />
    </Array>
  </mxGeometry>
</mxCell>
```

## 레이아웃 팁

1. **수직 흐름**: 위에서 아래로 진행
2. **분기 방향**: Yes는 아래, No는 오른쪽
3. **정렬**: 주 흐름은 중앙에, 분기는 좌우로
4. **간격**: 단계 간 80-100px
5. **루프백**: 오른쪽으로 돌아가는 곡선
6. **레이블 위치**: 분기선에 Yes/No 명시
