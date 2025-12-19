# Architecture 다이어그램 패턴

## 개요

시스템 아키텍처, 클라우드 인프라, 컴포넌트 구조를 표현하는 다이어그램.

## 레이어 구조

### 3-Tier 아키텍처

```
[Client Layer]     - 브라우저, 모바일 앱
      ↓
[Application Layer] - API 서버, 비즈니스 로직
      ↓
[Data Layer]       - 데이터베이스, 캐시
```

### 레이어별 Y 좌표 (권장)
- Client: y=40
- Application: y=180
- Data: y=320

## 스타일 가이드

### 레이어별 색상
```
Client:      fillColor=#E3F2FD (연한 파랑)
Application: fillColor=#E8F5E9 (연한 초록)
Data:        fillColor=#FFF3E0 (연한 주황)
External:    fillColor=#F5F5F5 (연한 회색)
```

### 컴포넌트별 도형

| 컴포넌트 | shape | 스타일 |
|----------|-------|--------|
| 웹 브라우저 | 기본 | rounded=1 |
| 서버/API | 기본 | rounded=0 |
| 데이터베이스 | cylinder3 | - |
| 캐시 | 기본 | dashed=1 |
| 외부 서비스 | cloud | - |
| 사용자 | actor | - |
| 로드밸런서 | ellipse | - |

## XML 템플릿

### 3-Tier 기본 구조

```xml
<mxfile host="app.diagrams.net" compressed="false">
  <diagram name="Architecture" id="arch-1">
    <!-- defaultFontFamily: 한글 폰트 지정 (예: Noto Sans KR, Malgun Gothic) -->
    <mxGraphModel dx="1434" dy="836" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="827" pageHeight="1169">
      <root>
        <mxCell id="0" />
        <mxCell id="1" parent="0" />

        <!-- Connections (edge를 먼저 기술 - 최배면) -->
        <mxCell id="7" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;" edge="1" parent="1" source="2" target="4">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="8" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;" edge="1" parent="1" source="3" target="4">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="9" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;startArrow=classic;" edge="1" parent="1" source="4" target="5">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="10" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;startArrow=classic;dashed=1;" edge="1" parent="1" source="4" target="6">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>

        <!-- Client Layer -->
        <mxCell id="2" value="Web Browser" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#E3F2FD;strokeColor=#1976D2;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="160" y="40" width="120" height="60" as="geometry" />
        </mxCell>
        <mxCell id="3" value="Mobile App" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#E3F2FD;strokeColor=#1976D2;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="320" y="40" width="120" height="60" as="geometry" />
        </mxCell>

        <!-- Application Layer -->
        <mxCell id="4" value="API Server" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#E8F5E9;strokeColor=#388E3C;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="240" y="180" width="120" height="60" as="geometry" />
        </mxCell>

        <!-- Data Layer -->
        <mxCell id="5" value="Database" style="shape=cylinder3;whiteSpace=wrap;html=1;fillColor=#FFF3E0;strokeColor=#F57C00;size=15;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="200" y="320" width="80" height="80" as="geometry" />
        </mxCell>
        <mxCell id="6" value="Cache" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#FFF3E0;strokeColor=#F57C00;dashed=1;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="320" y="340" width="80" height="40" as="geometry" />
        </mxCell>
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>
```

## 마이크로서비스 패턴

### 서비스 간 통신

```xml
<!-- API Gateway -->
<mxCell id="gw" value="API Gateway" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#E8F5E9;strokeColor=#388E3C;fontStyle=1;" vertex="1" parent="1">
  <mxGeometry x="240" y="100" width="120" height="60" as="geometry" />
</mxCell>

<!-- Services -->
<mxCell id="svc1" value="User Service" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#FFFFFF;strokeColor=#1976D2;" vertex="1" parent="1">
  <mxGeometry x="80" y="220" width="100" height="50" as="geometry" />
</mxCell>
<mxCell id="svc2" value="Order Service" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#FFFFFF;strokeColor=#1976D2;" vertex="1" parent="1">
  <mxGeometry x="240" y="220" width="100" height="50" as="geometry" />
</mxCell>
<mxCell id="svc3" value="Payment Service" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#FFFFFF;strokeColor=#1976D2;" vertex="1" parent="1">
  <mxGeometry x="400" y="220" width="100" height="50" as="geometry" />
</mxCell>

<!-- Message Queue -->
<mxCell id="mq" value="Message Queue" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#FCE4EC;strokeColor=#C2185B;dashed=1;" vertex="1" parent="1">
  <mxGeometry x="200" y="320" width="200" height="40" as="geometry" />
</mxCell>
```

## 클라우드 인프라 패턴

### 그룹 박스 (영역 표시)

```xml
<!-- VPC/Region 영역 -->
<mxCell id="vpc" value="VPC" style="rounded=0;whiteSpace=wrap;html=1;fillColor=none;strokeColor=#1976D2;dashed=1;strokeWidth=2;verticalAlign=top;fontStyle=1;" vertex="1" parent="1">
  <mxGeometry x="40" y="40" width="400" height="300" as="geometry" />
</mxCell>

<!-- Subnet -->
<mxCell id="subnet" value="Private Subnet" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#E3F2FD;strokeColor=#1976D2;opacity=50;verticalAlign=top;" vertex="1" parent="1">
  <mxGeometry x="60" y="80" width="160" height="200" as="geometry" />
</mxCell>
```

## 팁

1. **레이어 정렬**: 같은 레이어의 컴포넌트는 수평 정렬
2. **그룹화**: 관련 컴포넌트는 점선 박스로 그룹화
3. **데이터 흐름**: 위에서 아래로 (Client → Server → DB)
4. **색상 일관성**: 같은 유형은 같은 색상 사용
5. **레이블**: 컴포넌트명은 간결하게, 필요시 부가 설명

---

## AWS 다이어그램

### draw.io에서 AWS 아이콘 활성화

**방법 1: 내장 라이브러리**
1. 좌측 패널 하단 "More Shapes" 클릭
2. Networking 섹션에서 AWS 라이브러리 선택
3. Apply

**방법 2: 최신 아이콘 라이브러리 (2024)**
```
File → Open Library from → URL
https://raw.githubusercontent.com/m-radzikowski/diagrams-aws-icons/master/20240206/AWS%20Architecture%20Icons%2020240206.xml
```

### AWS 그룹 계층 구조

```
AWS Cloud
└── Region (ap-northeast-2)
    └── VPC
        ├── Availability Zone (AZ-a)
        │   ├── Public Subnet
        │   └── Private Subnet
        └── Availability Zone (AZ-b)
            ├── Public Subnet
            └── Private Subnet
```

### AWS 그룹 스타일

| 그룹 | 색상 | 스타일 |
|------|------|--------|
| AWS Cloud | 테두리만 | `strokeColor=#232F3E;fillColor=none;dashed=0;strokeWidth=2` |
| Region | 연한 파랑 | `fillColor=#E7F3FF;strokeColor=#147EBA;dashed=1` |
| VPC | 연한 초록 | `fillColor=#E9F7EF;strokeColor=#248814` |
| Availability Zone | 연한 파랑 | `fillColor=#E7F3FF;strokeColor=#147EBA;dashed=1` |
| Public Subnet | 연한 초록 | `fillColor=#E9F7EF;strokeColor=#248814` |
| Private Subnet | 연한 파랑 | `fillColor=#E7F3FF;strokeColor=#147EBA` |

### AWS 서비스 카테고리별 색상

| 카테고리 | 색상 | 서비스 예시 |
|----------|------|-------------|
| Compute | 주황 `#FF9900` | EC2, Lambda, ECS, Fargate |
| Database | 파랑 `#3B48CC` | RDS, DynamoDB, ElastiCache |
| Storage | 초록 `#3F8624` | S3, EBS, EFS |
| Networking | 보라 `#8C4FFF` | VPC, Route53, CloudFront, ALB |
| Security | 빨강 `#DD344C` | IAM, Cognito, WAF, KMS |
| Application | 분홍 `#E7157B` | API Gateway, SQS, SNS |

### AWS 3-Tier 레퍼런스 아키텍처

```xml
<mxfile host="app.diagrams.net" compressed="false">
  <diagram name="AWS-3Tier" id="aws-3tier">
    <!-- defaultFontFamily: 한글 폰트 지정 (예: Noto Sans KR, Malgun Gothic) -->
    <mxGraphModel dx="1434" dy="836" grid="1" gridSize="10" guides="1" tooltips="1" connect="1" arrows="1" fold="1" page="1" pageScale="1" pageWidth="1169" pageHeight="827">
      <root>
        <mxCell id="0" />
        <mxCell id="1" parent="0" />

        <!-- Connections (edge를 먼저 기술 - 최배면) -->
        <mxCell id="e1" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;" edge="1" parent="1" source="alb" target="ec2-a">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="e2" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;" edge="1" parent="1" source="alb" target="ec2-b">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="e3" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;startArrow=classic;" edge="1" parent="1" source="ec2-a" target="rds">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="e4" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;startArrow=classic;" edge="1" parent="1" source="ec2-b" target="rds">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>
        <mxCell id="e5" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;dashed=1;endArrow=classic;startArrow=classic;" edge="1" parent="1" source="rds" target="rds-standby">
          <mxGeometry relative="1" as="geometry" />
        </mxCell>

        <!-- AWS Cloud -->
        <mxCell id="aws" value="AWS Cloud" style="rounded=0;whiteSpace=wrap;html=1;fillColor=none;strokeColor=#232F3E;strokeWidth=2;verticalAlign=top;fontStyle=1;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="20" y="20" width="700" height="500" as="geometry" />
        </mxCell>

        <!-- Region -->
        <mxCell id="region" value="ap-northeast-2" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#E7F3FF;strokeColor=#147EBA;dashed=1;verticalAlign=top;fontStyle=1;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="40" y="60" width="660" height="440" as="geometry" />
        </mxCell>

        <!-- VPC -->
        <mxCell id="vpc" value="VPC (10.0.0.0/16)" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#E9F7EF;strokeColor=#248814;verticalAlign=top;fontStyle=1;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="60" y="100" width="620" height="380" as="geometry" />
        </mxCell>

        <!-- Public Subnet AZ-a -->
        <mxCell id="pub-a" value="Public Subnet&#xa;10.0.1.0/24" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#E9F7EF;strokeColor=#248814;verticalAlign=top;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="80" y="140" width="140" height="140" as="geometry" />
        </mxCell>

        <!-- Public Subnet AZ-b -->
        <mxCell id="pub-b" value="Public Subnet&#xa;10.0.2.0/24" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#E9F7EF;strokeColor=#248814;verticalAlign=top;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="240" y="140" width="140" height="140" as="geometry" />
        </mxCell>

        <!-- Private Subnet AZ-a -->
        <mxCell id="priv-a" value="Private Subnet&#xa;10.0.3.0/24" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#E7F3FF;strokeColor=#147EBA;verticalAlign=top;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="80" y="300" width="140" height="160" as="geometry" />
        </mxCell>

        <!-- Private Subnet AZ-b -->
        <mxCell id="priv-b" value="Private Subnet&#xa;10.0.4.0/24" style="rounded=0;whiteSpace=wrap;html=1;fillColor=#E7F3FF;strokeColor=#147EBA;verticalAlign=top;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="240" y="300" width="140" height="160" as="geometry" />
        </mxCell>

        <!-- ALB -->
        <mxCell id="alb" value="ALB" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#8C4FFF;strokeColor=#5429A6;fontColor=#FFFFFF;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="140" y="170" width="80" height="40" as="geometry" />
        </mxCell>

        <!-- EC2 Instances -->
        <mxCell id="ec2-a" value="EC2" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#FF9900;strokeColor=#CC7A00;fontColor=#FFFFFF;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="110" y="350" width="80" height="40" as="geometry" />
        </mxCell>
        <mxCell id="ec2-b" value="EC2" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#FF9900;strokeColor=#CC7A00;fontColor=#FFFFFF;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="270" y="350" width="80" height="40" as="geometry" />
        </mxCell>

        <!-- RDS -->
        <mxCell id="rds" value="RDS&#xa;Primary" style="shape=cylinder3;whiteSpace=wrap;html=1;fillColor=#3B48CC;strokeColor=#2E3A9F;fontColor=#FFFFFF;size=10;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="440" y="320" width="80" height="80" as="geometry" />
        </mxCell>
        <mxCell id="rds-standby" value="RDS&#xa;Standby" style="shape=cylinder3;whiteSpace=wrap;html=1;fillColor=#3B48CC;strokeColor=#2E3A9F;fontColor=#FFFFFF;size=10;opacity=60;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="560" y="320" width="80" height="80" as="geometry" />
        </mxCell>

        <!-- S3 -->
        <mxCell id="s3" value="S3" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#3F8624;strokeColor=#2D6119;fontColor=#FFFFFF;fontSize=18;" vertex="1" parent="1">
          <mxGeometry x="500" y="180" width="80" height="40" as="geometry" />
        </mxCell>
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>
```

### AWS Serverless 패턴

```xml
<!-- API Gateway -->
<mxCell id="apigw" value="API Gateway" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#E7157B;strokeColor=#B01161;fontColor=#FFFFFF;" vertex="1" parent="1">
  <mxGeometry x="100" y="100" width="100" height="40" as="geometry" />
</mxCell>

<!-- Lambda -->
<mxCell id="lambda" value="Lambda" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#FF9900;strokeColor=#CC7A00;fontColor=#FFFFFF;" vertex="1" parent="1">
  <mxGeometry x="100" y="200" width="100" height="40" as="geometry" />
</mxCell>

<!-- DynamoDB -->
<mxCell id="dynamodb" value="DynamoDB" style="shape=cylinder3;whiteSpace=wrap;html=1;fillColor=#3B48CC;strokeColor=#2E3A9F;fontColor=#FFFFFF;size=10;" vertex="1" parent="1">
  <mxGeometry x="100" y="300" width="100" height="70" as="geometry" />
</mxCell>

<!-- SQS -->
<mxCell id="sqs" value="SQS" style="rounded=1;whiteSpace=wrap;html=1;fillColor=#E7157B;strokeColor=#B01161;fontColor=#FFFFFF;" vertex="1" parent="1">
  <mxGeometry x="260" y="200" width="80" height="40" as="geometry" />
</mxCell>
```

### AWS 아이콘 Best Practices

1. **공식 색상 사용**: AWS 서비스 카테고리별 공식 색상 준수
2. **그룹 중첩**: AWS Cloud > Region > VPC > AZ > Subnet 순서
3. **버퍼 유지**: 중첩된 그룹 간 최소 20px 여백
4. **레이블 위치**: 그룹은 `verticalAlign=top`, 서비스는 중앙
5. **아이콘 일관성**: 같은 서비스는 같은 스타일 (한 다이어그램 내 EC2를 다른 아이콘으로 표현 금지)
6. **흐름 방향**: 좌→우 또는 상→하 (Client → ALB → EC2 → RDS)

---

## AWS 서브넷 배치 전략

### 계층별 서브넷 구분

| 계층 | 서브넷 유형 | 배치 리소스 | 색상 |
|------|------------|------------|------|
| 진입 | Public | ALB, NAT Gateway, Bastion | 초록 `#E9F7EF` |
| 애플리케이션 | Private (App) | EC2, ECS, Lambda | 파랑 `#E7F3FF` |
| 데이터 | Private (Data) | RDS, ElastiCache, OpenSearch | 파랑 `#E7F3FF` |

### ID 네이밍 규칙

복잡한 아키텍처에서 리소스 추적을 위해 **구조화된 ID** 사용:

```
{유형}-{서비스}-{AZ}-{번호}
```

**예시**:
```xml
<!-- 서브넷 -->
<mxCell id="subnet-pub-a-1" value="Public Subnet" ... />
<mxCell id="subnet-priv-a-app" value="App Subnet" ... />
<mxCell id="subnet-priv-a-data" value="Data Subnet" ... />

<!-- 서비스 -->
<mxCell id="svc-alb-1" value="ALB" ... />
<mxCell id="svc-ec2-a-1" value="EC2" ... />
<mxCell id="svc-rds-primary" value="RDS Primary" ... />
```

### 메타 정보 표현 방식

#### 방법 1: value 속성에 메타 정보 포함

```xml
<mxCell id="subnet-pub-a"
        value="Public Subnet&#xa;10.0.1.0/24&#xa;[IGW, NAT]"
        style="..." />
```

#### 방법 2: 주석으로 메타 정보 관리

```xml
<!--
  META: subnet-pub-a
  CIDR: 10.0.1.0/24
  Resources: ALB, NAT Gateway
  Inbound: IGW
  Outbound: Internet
-->
<mxCell id="subnet-pub-a" value="Public Subnet" ... />
```

#### 방법 3: 별도 메타 문서 (권장 - 복잡한 경우)

`aws-meta.yaml`:
```yaml
subnets:
  subnet-pub-a:
    type: public
    cidr: 10.0.1.0/24
    az: ap-northeast-2a
    resources: [ALB, NAT-GW]
    routes:
      - dest: 0.0.0.0/0
        target: igw

  subnet-priv-a-app:
    type: private
    cidr: 10.0.3.0/24
    az: ap-northeast-2a
    resources: [EC2, ECS]
    routes:
      - dest: 0.0.0.0/0
        target: nat-gw
      - dest: 10.0.0.0/16
        target: local
```

---

## AWS 진입 경로 표현

### 표준 진입 패턴

```
Internet
    │
    ▼
┌─────────────────────────────────┐
│ Internet Gateway                │
└─────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────┐ Public Subnet
│ ALB (Application Load Balancer) │
└─────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────┐ Private Subnet (App)
│ EC2 / ECS / Lambda              │
└─────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────┐ Private Subnet (Data)
│ RDS / ElastiCache               │
└─────────────────────────────────┘
```

### 화살표 스타일 가이드

| 트래픽 유형 | 스타일 | 예시 |
|------------|--------|------|
| 인터넷 → 내부 | 굵은 실선 `strokeWidth=2` | IGW → ALB |
| 내부 통신 | 일반 실선 | ALB → EC2 |
| DB 연결 | 양방향 화살표 `startArrow=classic` | EC2 ↔ RDS |
| 복제/동기화 | 점선 `dashed=1` | RDS Primary ↔ Standby |
| 비동기/이벤트 | 열린 화살표 `endArrow=open` | Lambda → SQS |

### 연결선 XML 패턴

```xml
<!-- 인터넷 진입 (굵은 선) -->
<mxCell id="e-igw-alb" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;strokeWidth=2;strokeColor=#232F3E;" edge="1" parent="1" source="igw" target="alb">
  <mxGeometry relative="1" as="geometry" />
</mxCell>

<!-- 내부 통신 -->
<mxCell id="e-alb-ec2" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;" edge="1" parent="1" source="alb" target="ec2">
  <mxGeometry relative="1" as="geometry" />
</mxCell>

<!-- DB 양방향 -->
<mxCell id="e-ec2-rds" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;startArrow=classic;" edge="1" parent="1" source="ec2" target="rds">
  <mxGeometry relative="1" as="geometry" />
</mxCell>

<!-- 복제 (점선) -->
<mxCell id="e-rds-replica" style="edgeStyle=orthogonalEdgeStyle;rounded=0;html=1;endArrow=classic;startArrow=classic;dashed=1;strokeColor=#757575;" edge="1" parent="1" source="rds-primary" target="rds-standby">
  <mxGeometry relative="1" as="geometry" />
</mxCell>
```

---

## 요구사항별 아키텍처 패턴

### 고가용성 (HA) 패턴

```
Region
├── AZ-a
│   ├── Public: ALB, NAT-GW
│   ├── Private-App: EC2/ECS
│   └── Private-Data: RDS Primary
└── AZ-b
    ├── Public: ALB, NAT-GW
    ├── Private-App: EC2/ECS
    └── Private-Data: RDS Standby
```

**다이어그램 표현**:
- 각 AZ를 점선 박스로 구분 (`dashed=1`)
- AZ 간 복제는 점선 화살표
- 동일 서비스는 같은 Y 좌표에 배치

### 보안 강화 패턴

```
Internet
    ↓
  [WAF] ─────────────────────┐
    ↓                        │
  [CloudFront] ──────────────┼─→ S3 (Static)
    ↓                        │
  [ALB] ←────────────────────┘
    ↓
  [Security Group: Web-SG]
    ↓
  [EC2/ECS]
    ↓
  [Security Group: App-SG]
    ↓
  [RDS]
```

**다이어그램 표현**:
- Security Group은 점선 박스로 감싸기
- WAF/Shield는 외곽에 배치
- Network ACL은 서브넷 경계에 레이블

### VPN/Direct Connect 패턴

```
On-Premises
    │
    ├─[VPN]──────→ VGW ──→ Private Subnet
    │                         │
    └─[DX]───────→ DX-GW ─────┘
```

**다이어그램 표현**:
- On-Premises는 별도 그룹 (회색 `#F5F5F5`)
- VPN 연결은 점선
- Direct Connect는 굵은 실선

### 비용 최적화 패턴

```
Region
├── AZ-a
│   ├── Public: NAT-GW (공유)
│   └── Private: EC2 Spot + Reserved
└── AZ-b
    ├── Public: (NAT-GW 공유)
    └── Private: EC2 Spot + Reserved
```

**다이어그램 표현**:
- 공유 리소스는 AZ 경계에 걸쳐 배치
- Spot 인스턴스: 점선 테두리 `dashed=1`
- Reserved: 실선 + 라벨 "[Reserved]"

---

## 컴포넌트 기반 다이어그램 관리

### 개요

복잡한 아키텍처를 작은 단위로 분리하여 관리하고, 최종 결과물로 합치는 방식.

### 디렉토리 구조

```
docs/diagrams/
├── aws-architecture.drawio      # 최종 결과물
├── components/                  # 재사용 가능한 컴포넌트
│   ├── subnet-public.drawio
│   ├── subnet-private-app.drawio
│   ├── subnet-private-data.drawio
│   ├── alb-cluster.drawio
│   └── rds-ha.drawio
├── manifest.yaml                # 선언적 정의
└── build.py                     # 조합 스크립트 (선택)
```

### 컴포넌트 설계 원칙

1. **단일 책임**: 하나의 컴포넌트는 하나의 논리 단위만
2. **명확한 경계**: 입력/출력 연결점 정의
3. **독립적 테스트**: 컴포넌트별 독립 검증 가능
4. **버전 관리**: Git으로 변경 이력 추적

### manifest.yaml 예시

```yaml
# 선언적 아키텍처 정의
name: production-architecture
version: 1.0.0

# 컴포넌트 정의
components:
  vpc:
    source: components/vpc-base.drawio
    position: { x: 40, y: 60 }

  public-subnet-a:
    source: components/subnet-public.drawio
    position: { x: 80, y: 140 }
    parent: vpc
    params:
      cidr: 10.0.1.0/24
      az: ap-northeast-2a

  public-subnet-b:
    source: components/subnet-public.drawio
    position: { x: 240, y: 140 }
    parent: vpc
    params:
      cidr: 10.0.2.0/24
      az: ap-northeast-2b

  private-subnet-a:
    source: components/subnet-private-app.drawio
    position: { x: 80, y: 300 }
    parent: vpc
    params:
      cidr: 10.0.3.0/24

  rds-cluster:
    source: components/rds-ha.drawio
    position: { x: 440, y: 300 }
    parent: vpc

# 연결 정의
connections:
  - from: alb
    to: ec2-cluster
    style: default

  - from: ec2-cluster
    to: rds-cluster
    style: bidirectional

  - from: rds-cluster.primary
    to: rds-cluster.standby
    style: replication
```

### draw.io 멀티 페이지 활용

하나의 `.drawio` 파일에 여러 다이어그램 포함:

```xml
<mxfile host="app.diagrams.net" compressed="false">
  <!-- 전체 아키텍처 -->
  <diagram name="Overview" id="overview">
    ...
  </diagram>

  <!-- Public Subnet 상세 -->
  <diagram name="Public-Subnet" id="pub-subnet">
    ...
  </diagram>

  <!-- Private Subnet 상세 -->
  <diagram name="Private-Subnet" id="priv-subnet">
    ...
  </diagram>

  <!-- RDS 클러스터 상세 -->
  <diagram name="RDS-Cluster" id="rds">
    ...
  </diagram>
</mxfile>
```

**장점**:
- 단일 파일로 관리
- draw.io 탭으로 전환
- 컴포넌트 간 참조 용이

### 컴포넌트 템플릿 예시

`components/subnet-public.drawio`:
```xml
<mxfile host="app.diagrams.net" compressed="false">
  <diagram name="Public-Subnet-Template" id="pub-tpl">
    <mxGraphModel>
      <root>
        <mxCell id="0" />
        <mxCell id="1" parent="0" />

        <!-- 서브넷 박스 -->
        <mxCell id="subnet" value="Public Subnet&#xa;{{CIDR}}"
                style="rounded=0;whiteSpace=wrap;html=1;fillColor=#E9F7EF;strokeColor=#248814;verticalAlign=top;"
                vertex="1" parent="1">
          <mxGeometry x="0" y="0" width="140" height="140" as="geometry" />
        </mxCell>

        <!-- 연결점 (다른 컴포넌트와 연결) -->
        <!-- IN: 상단 중앙 -->
        <mxCell id="port-in" value=""
                style="ellipse;fillColor=#4CAF50;strokeColor=#388E3C;"
                vertex="1" parent="subnet">
          <mxGeometry x="65" y="-5" width="10" height="10" as="geometry" />
        </mxCell>

        <!-- OUT: 하단 중앙 -->
        <mxCell id="port-out" value=""
                style="ellipse;fillColor=#2196F3;strokeColor=#1976D2;"
                vertex="1" parent="subnet">
          <mxGeometry x="65" y="135" width="10" height="10" as="geometry" />
        </mxCell>
      </root>
    </mxGraphModel>
  </diagram>
</mxfile>
```

### 조합 도구 (drawio-tools.py 확장)

```bash
# 컴포넌트 유효성 검사
python scripts/drawio-tools.py validate components/subnet-public.drawio

# manifest 기반 조합 (향후 구현)
python scripts/drawio-tools.py build manifest.yaml -o aws-architecture.drawio

# 컴포넌트 목록 확인
python scripts/drawio-tools.py list components/
```

### 워크플로우

```
1. 컴포넌트 설계
   └─ 재사용 가능한 단위로 분리

2. 개별 컴포넌트 작성
   └─ components/*.drawio 생성

3. 선언적 정의
   └─ manifest.yaml 작성

4. 검증
   └─ drawio-tools.py validate

5. 조합 (수동 또는 자동)
   └─ 최종 다이어그램 생성

6. 버전 관리
   └─ Git commit
```
