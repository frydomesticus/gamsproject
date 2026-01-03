$TITLE Makale Modeli - Senaryo 1 (Ayrı iade aracı) - Çalışan GAMS

* =========================================================
* MAKALENİN VERİLERİ (Tablo 1-4) + Model (MILP)
* Senaryo 1: 3 dağıtım aracı (a1..a3) + 1 iade aracı (ay1)
* Zaman: 16 gün
* =========================================================

SETS
    t   "Gün"                                / t1*t16 /
    m   "Tesisler: fabrika + dağıtım merkezleri" / m0*m5 /
    md(m) "Dağıtım merkezleri"               / m1*m5 /
    a   "Dağıtım araçları"                   / a1*a3 /
    ay  "İade araçları"                      / ay1 / ;

ALIAS(t,tp);

PARAMETER tpos(t) "t indisinin sayısal karşılığı";
tpos(t) = ord(t);

* -----------------------------
* TALEP (Tablo 3)
* -----------------------------
PARAMETER d(md,t) "Talep (lt)";

TABLE d(md,t)
         t1    t2    t3    t4    t5    t6    t7    t8    t9    t10   t11   t12   t13   t14   t15   t16
m1       900   9800  5900  6500  4500  4900  3100  1900  18500 13700 12800 17500 13100 6800  5900  1900
m2       6800  5300  4600  6700  7000  7500  2100  7700  6300  2400  1300  4600  7400  1900  7000  1700
m3       1500  900   3400  2800  200   11800 2000  3300  2100  7400  9900  5500  4400  3300  5800  11200
m4       8900  4700  400   7700  3000  7100  2200  7600  2500  1300  2400  700   3000  3700  8200  500
m5       4000  4400  2900  2100  1600  1100  5100  4500  1900  2200  2600  6400  2900  3300  5600  7000 ;

* -----------------------------
* İADE (Tablo 4)
* -----------------------------
PARAMETER b(md,t) "İade (lt)";

TABLE b(md,t)
         t1   t2   t3   t4   t5   t6   t7   t8   t9   t10  t11  t12  t13  t14  t15  t16
m1       1500 1800 2000 100  1000 300  1500 100  1300 300  1900 900  1200 1600 700  300
m2       500  400  700  400  100  700  700  200  700  0    600  600  100  200  400  100
m3       900  1000 1300 600  700  400  1200 800  500  300  800  800  1400 1300 900  1000
m4       1700 1500 1800 1800 1300 300  100  300  100  400  1300 1300 900  1700 0    1200
m5       900  600  800  500  1200 400  600  1000 600  800  800  700  800  0    100  600 ;

* -----------------------------
* MAKİLEDEKİ SABİTLER (metin)
* -----------------------------
SCALAR
    fpc  /150/
    vpc  /1.25/
    pmin /20000/
    pmax /66000/
    q    /19800/
    ftc  /255/
    frc  /300/
    g    /0.25/
    sl   /8/
    rl   /3/ ;

* -----------------------------
* Tablo 1: stok maliyeti h(m) ve depo alanı Alan(m)
* -----------------------------
PARAMETER h(m) "Stok maliyeti", Alan(m) "Depo alanı";

h("m0") = 0.00042;  h("m1") = 0.00037;  h("m2") = 0.00038;
h("m3") = 0.00035;  h("m4") = 0.00035;  h("m5") = 0.00037;

Alan("m0") = 380000;  Alan("m1") = 100000;  Alan("m2") = 60000;
Alan("m3") =  90000;  Alan("m4") =  80000;  Alan("m5") = 40000;

* -----------------------------
* Tablo 2: dönem başı stoklar
* -----------------------------
PARAMETER Sinit(m) "Başlangıç kullanılabilir stok", BSinit(md) "Başlangıç iade stok";

Sinit("m0") = 6200;  Sinit("m1") = 1200;  Sinit("m2") = 7100;
Sinit("m3") = 3000;  Sinit("m4") = 9400;  Sinit("m5") = 4300;

BSinit("m1") = 1200; BSinit("m2") = 400; BSinit("m3") = 1000;
BSinit("m4") = 800;  BSinit("m5") = 300;

* -----------------------------
* Üst stok sınırları (raf ömrü mantığı)
* -----------------------------
PARAMETER um(md,t), u0(t), ubm(md,t);

um(md,t)  = SUM(tp$(tpos(tp) >= tpos(t) AND tpos(tp) <= tpos(t) + sl - 1), d(md,tp));
u0(t)     = SUM(md, um(md,t));
ubm(md,t) = SUM(tp$(tpos(tp) >= tpos(t) AND tpos(tp) <= tpos(t) + rl - 1), b(md,tp));

* =========================================================
* KARAR DEĞİŞKENLERİ
* =========================================================
BINARY VARIABLES
    Z(t)          "Üretim var/yok"
    X(md,a,t)     "a aracı md’ye gider mi?"
    Y(a,t)        "a aracı kullanıldı mı?"
    F(md,ay,t)    "ay aracı md’den iade alır mı?"
    RY(ay,t)      "ay aracı geri işleme merkezine gider mi?";

POSITIVE VARIABLES
    P(t)          "Üretim miktarı"
    S0(t)         "Fabrika stok"
    S(md,t)       "DM stok"
    BS(md,t)      "DM iade stok"
    GS(m,t)       "Toplam stok"
    W(md,a,t)     "Sevk miktarı"
    V(md,ay,t)    "Toplanan iade miktarı";

VARIABLE ObjZ;

* =========================================================
* DENKLEMLER
* =========================================================
EQUATIONS
    OBJ
    InvF(t)
    InvD(md,t)
    InvRet(md,t)
    CapF(t)
    CapD(md,t)
    CapRet(md,t)
    DefGS0(t)
    DefGSd(md,t)
    Area0(t)
    Areadd(md,t)
    ProdMax(t)
    ProdMin(t)
    VehCapFwd(md,a,t)
    OneTripFwd(md,t)
    LinkUseFwd(a,t)
    VehCapRev(md,ay,t)
    OneTripRev(md,t)
    LinkRecycle(ay,t)
    NonNegS0(t)
    NonNegS(md,t)
    NonNegBS(md,t);

* Amaç fonksiyonu (makaledeki yapı)
OBJ..
ObjZ =E=
    SUM(t, fpc*Z(t) + vpc*P(t))
  + SUM((m,t), h(m)*GS(m,t))
  + SUM((a,t), ftc*Y(a,t))
  + SUM((ay,t), frc*RY(ay,t))
  - SUM((md,ay,t), g*V(md,ay,t));

* Fabrika stok dengesi
InvF(t)..
S0(t) =E=
    (Sinit("m0")$(ord(t)=1) + S0(t-1)$(ord(t)>1))
  + P(t)
  - SUM((md,a), W(md,a,t));

* DM stok dengesi
InvD(md,t)..
S(md,t) =E=
    (Sinit(md)$(ord(t)=1) + S(md,t-1)$(ord(t)>1))
  + SUM(a, W(md,a,t))
  - d(md,t);

* İade stok dengesi
InvRet(md,t)..
BS(md,t) =E=
    (BSinit(md)$(ord(t)=1) + BS(md,t-1)$(ord(t)>1))
  + b(md,t)
  - SUM(ay, V(md,ay,t));

* Üst stok kısıtları
CapF(t)..     S0(t) =L= u0(t);
CapD(md,t)..  S(md,t) =L= um(md,t);
CapRet(md,t)..BS(md,t) =L= ubm(md,t);

* Toplam stok tanımları (fabrika ve DM ayrı)
DefGS0(t)..
GS("m0",t) =E= S0(t);

DefGSd(md,t)..
GS(md,t) =E= S(md,t) + BS(md,t);

* Depo alanı kısıtları
Area0(t)..     GS("m0",t) =L= Alan("m0");
Areadd(md,t).. GS(md,t)   =L= Alan(md);

* Üretim kapasitesi
ProdMax(t).. P(t) =L= pmax*Z(t);
ProdMin(t).. P(t) =G= pmin*Z(t);

* Dağıtım araç kapasitesi ve ziyaret
VehCapFwd(md,a,t).. W(md,a,t) =L= q*X(md,a,t);
OneTripFwd(md,t)..  SUM(a, X(md,a,t)) =L= 1;
LinkUseFwd(a,t)..   SUM(md, X(md,a,t)) =L= CARD(md)*Y(a,t);

* İade aracı kapasitesi ve ziyaret
VehCapRev(md,ay,t).. V(md,ay,t) =L= q*F(md,ay,t);
OneTripRev(md,t)..   SUM(ay, F(md,ay,t)) =L= 1;
LinkRecycle(ay,t)..  SUM(md, F(md,ay,t)) =L= CARD(md)*RY(ay,t);

* Negatif stok olmasın (okunabilirlik için açık yazıyoruz)
NonNegS0(t)..  S0(t) =G= 0;
NonNegS(md,t)..S(md,t) =G= 0;
NonNegBS(md,t)..BS(md,t)=G= 0;

MODEL MakaleSenaryo1 / ALL /;

OPTION MIP = CPLEX;
OPTION OPTCR = 0.00;

SOLVE MakaleSenaryo1 USING MIP MINIMIZING ObjZ;

DISPLAY ObjZ.L;
DISPLAY P.L, Z.L;
DISPLAY S0.L, S.L, BS.L;
DISPLAY X.L, W.L, Y.L;
DISPLAY F.L, V.L, RY.L;
