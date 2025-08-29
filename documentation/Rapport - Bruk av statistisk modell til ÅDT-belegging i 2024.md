---
aliases:
  - ÅDT2024 rapport
---
# *Introduksjon*

Formålet med dette notatet er å gi en utdypende oversikt over modellen som ble brukt til ÅDT-belegging for 2024 -- helt fra datagrunnlaget til hvordan modellen ble bygd opp og evaluert. Notatet vil også beskrive resultatene av modellen og hvordan disse resultatene ble korrigert manuelt i ettertid. Dette dekker også grove trekk rundt hvilke endringer som ble gjort og på hvilke steder de mest vesentlige korrigeringene ble gjort. Fra dette vil vi drøfte de største svakhetene ved ÅDT-belegginga for 2024 i alle steg, både i datakildene og modelleringa. 

# *Prosedyre for ÅDT-belegging for 2024*

ÅDT-modellen tar utgangspunkt i en del ulike datakilder som alle gir et ÅDT-tall som brukes som respons-verdi i en statistisk modell. Denne modellen tar også inn ulike forklaringsvariabler som fins på alle trafikklenkene, som kan bidra til å forutsi trafikkmengden der hvor det ikke fins registreringspunkter. Den statistiske modellen inneholder også en romlig komponent som forklarer hvordan trafikkmengden i ei gitt trafikklenke avhenger av trafikkmengden i lenkene rundt. I tillegg til modellen som predikerer ÅDT for alle motorkjøretøy, ble det også utviklet en modell for å predikere tungbil-andel. I denne seksjonen beskriver vi datakildene som ligger til grunn, noen detaljer rundt trafikklenkegrafen, en omgående beskrivelse av den statistiske modellen og balanseringa som gjøres av de predikerte trafikkmengdene, og det gis en beskrivelse av modellen for tungbil-andeler. 
## Registreringer av trafikkmengder
Dataen som ligger til grunn for ÅDT-modellen kommer fra en rekke ulike typer av registreringer. Når datagrunnlaget for modellen skal velges, prioriteres de i følgende rekkefølge: 

| Registreringstype                                 | Antall    | Tungbilantall |
| ------------------------------------------------- | --------- | ------------- |
| 1. Kontinuerlig fra trafikkdatasystemet           | 5052      | 5049          |
| 2. Kontinuerlig registrering fra AUTOPASS         | 323       | 323           |
| 3. Kontinuerlig registrering fra eksterne kilder  | 0         | 0             |
| 4. Utleda verdi                                   | 974       | 952           |
| 5. Periodisk registrering fra trafikkdatasystemet | 5739      | 2921          |
| 6. Periodisk registrering fra eksterne kilder     | 895       | 895           |
| **Totalt**                                        | **12983** | **10140**     |

**Kontinuerlige målinger**
Den viktigste kilden er kontinuerlige registreringspunkter, hvor det står en sensor som kontinuerlig registrerer all trafikk som passerer den. Dersom sensoren er nede i en periode, vil det registreres en dekningsgrad som indikerer hvor stor andel av tida den faktisk registrerte kjøretøy (Statens vegvesen, u.å.). Usikkerheten angis som kvadratisk gjennomsnitt av avvikene mellom ÅDT-tallet og hvert enkelt døgnvolum, med en korrigering (finite population correction factor) som gjør at usikkerheten er null hvis ingen dager mangler data, og større hvis flere dager mangler data.

**Periodiske målinger**
I tillegg settes det enkelte steder opp periodiske registreringspunkt, hvor en sensor gjerne står i et par uker. ÅDT-mengden utledes da fra de periodiske målingene ved hjelp av såkalte faktor-kurver. Usikkerheten settes basert på avviket mellom målingene og faktor-kurven.

**AUTOPASS bomstasjoner**
Data fra bomstasjoner brukes også. Siden disse også er kontinuerlige settes usikkerheten til disse på samme måte som de kontinuerlige målingene fra TRP.

**Ferger og annen ekstern data**
Data for alle fergestrekninger er tilgjengelig fra Ferjedatabanken. I tillegg fins det noen ganger andre eksterne målinger gjort av kommuner. Disse får usikkerhet satt til 30% av ÅDT-tallet.

**Utleda verdier**
For noen kontinuerlige trafikklenker er det mulig å regne ut entydig trafikkmengde basert på omkringliggende registreringspunkter, for eksempel hvis det er en vei med en avkjøringsrampe, og det er registreringspunkt før rampa og på selve rampa. I så tilfelle kan trafikkmengen på vegen etter rampa utledes direkte som "trafikkmengde før rampe" - "trafikkmengde på rampe". De utleda verdiene regnes på retta trafikklenker. Usikkerheten til en utleda verdi settes til kvadratisk gjennomsnitt av de bidragende trafikklenkene.

**Data på tungbiler**
Mange av datakildene rapporterer også tungbilandeler, men disse baseres på ulike karakteristikker utifra hvordan registreringen gjøres. For kontinuerlige trafikkregistreringspunkt baseres klassifiseringen pålengde, kjøretøy over 5.6 m klassifiseres som tungbiler. AUTOPASS bruker offisiell klassifisering registrert i brikken, som reflekterer om kjøretøyet er over eller under 3.5 tonn. For de periodiske vil det trolig også baseres på lengde, bortsett fra de fra Ferjedatabanken som potensielt baserer seg på kjøretøyklassifisering fra AUTOPASS.

## Trafikklenkegrafen
Trafikklenkegrafen viser hvordan alle europa-, riks- og fylkesveier i Norge henger sammen. I tillegg er det også inkludert et par sentrale kommunale veier der det er nødvendig for å representere trafikkflyten mer riktig. Grafen er bygd opp av små segmenter som kalles trafikklenker, som er segmenter hvor det er rimelig å anta at trafikkmengden er konstant over. 

## ÅDT-modellen
#### **Den statistiske modellen**
Modellen er en såkalt GLMM (generalised linear mixed model) med ei Poisson-fordeling som rimelighetsfunksjon. Modellen som kobler de $n_e$ trafikkmengde-observasjonene $y_i$, $i = 1, \dots, n_e$, sammen med de $p$ forklaringsvariablene $x_j$, $j = 1, \dots, p$, og de andre modellkomponentene ser da slik ut:

$$
\begin{aligned}
y_i &~ \sim \text{Poisson}(\mu_i) \ , \\
\log(\mu_i) &= \beta_0 + \beta_1x_1 + \beta_2x_2 + \dots + \beta_px_p + u_i + r_i \ .
\end{aligned}
$$

I uttrykket over ser vi beskrevet hvordan en observert trafikkmengde $y_i$ er knyttet til ei Poisson-fordeling med intensitet $\mu_i$. Intensiteten er videre beskrevet som en funksjon av en rekke forklaringsvariabler, samt to komponenter $u_i$ og $r_i$. I dette tilfellet angir $u_i$ den romlige komponenten som tar inn trafikklenkegrafen, mens $r_i$ angir et ustrukturert variasjonsledd som sier at hvert vegsystem tillates å variere litt uavhengig av naboene. Dette siste leddet er nyttig som et ledd i en Poisson-modell når man antar at man kan ha over-spredning, altså at variansen og forventningsverdien ikke nødvendigvis er lik. 

Den endelige modellen brukte følgende formel: 
```
  formula <- prelimAadt ~ 
    f(link, model = "besag", graph = graph,
      adjust.for.con.comp = FALSE, 
	  scale.model = FALSE, 
	  constr = TRUE) +
    f(roadSystemReference, model = "iid")
    functionalRoadClass * maxLanes + 
    minLanes * highestSpeedLimit +
    minLanes * functionClass +
    highestSpeedLimit:urbanRatio + 
    roadCategory * numberOfEstablishments + 
    functionClass:numberOfEmployees +
    minLanes:roadCategory + 
    highestSpeedLimit:functionClass 
```

`f(link, model = "besag",...)` er den romlige komponenten, omtalt som $u_i$ i formelen. En Besag modell for en stokastisk vektor $\boldsymbol{u} = (u_1, \dots, u_{n_e})$ er definert som 

$$
u_i \mid u_j \sim \mathcal{N}(\frac{1}{n_i}\sum_{i\sim j} u_j, \frac{1}{n_i\tau}) \ ,
$$

for alle $i \neq j$. Her er $n_i$ antall naboer for node $i$, og notasjonen $i\sim j$ betyr at node $i$ og $j$ er naboer. Altså sier vi at verdien $u_i$ til node $i$ er normalfordelt rundt snittet til verdien hos nabo-nodene, med varians $1/n_i\tau$, hvor $\tau$ er en hyperparameter. Denne typen modell omtales også ofte som en conditional autoregressive modell (CAR)

`f(roadSystemReference, model = "iid")` er random effect leddet som tillater et ekstra variasjonsledd for hvert vegsystem, omtalt som $r_i$ i formelen. 

Til slutt kommer en rekke forklaringsvariabler. I denne formelen er de oppgitt som interaksjonseffekter, men her beskriver vi hver av dem individuelt.
- `functionalRoadClass` Funksjonell vegklasse, et klassifiseringssystem med ni kategorier
- `maxLanes` Maks antall felt på trafikklenka
- `minLanes` Minimum antall felt på trafikklenka
- `highestSpeedLimit` Høyeste fartsgrense på trafikklenka
- `functionClass` Funksjonsklasse, et klassifiseringssystem med fem kategorier
- `urbanRatio` Andel av lenka som ligger i et urbant område
- `roadCategory` Vegkategori: enten europavei, riksvei, fylkesvei, eller kommunal vei.
- `numberOfEstablishments` Antall bedrifter i nærheten av lenka
- `numberOfEmployees` Antall registrerte ansatte i nærheten av lenka

Dette settet med kovariater og interaksjoner ble valgt gjennom en kryssvalideringsprosedyre hvor alle mulige kombinasjoner av kovariater ble undersøkt. Kryssvalideringen gikk over $k = 5$ "folder" av dataen som ble valgt tilfeldig. Dette fungerer på den måten at først blir alle trafikklenkene delt tilfeldig inn i $k$ grupper, eller folder, og så trenes modellene på $k-1$ av de foldene, og testes på den siste av dem. Så lagrer man ulike ytelses-metrikker for hver av modellene. Til slutt regner man gjennomsnittet av metrikkene på tvers av alle foldene for hver modell, så man til slutt sitter igjen med ytelses-metrikker som kan brukes til å sammenligne modellene. På lignende vis ble det også undersøkt effekten av å prøve andre rimelighetsfunksjoner, og det var slik man valgte ei Poisson-fordeling over alternative muligheter. 

Når en modell skal tilpasses i INLA foregår dette i et såkalt Bayesiansk paradigme, som medfører at alle parametere får à priori fordelinger som deretter oppdateres i møtet med dataen. I dette tilfellet ble det brukt INLAs default verdier for alle parametere. Parameterne i denne modellen er alle koeffisientene til forklaringsvariablene, den inverse variansen til den romlige komponenten $u_i$, og den inverse variansen til $r_i$ (i INLA brukes invers varians, kalt presisjon). Regresjonskoeffisientene får som à priori fordeling $\mathcal{N}(0, 0.001)$ og skjæringspunktet $\beta_0$ får fordeling $\mathcal{N}(0, 0)$. For $u_i$ og $r_i$ defineres à priori-fordelingene på logaritmen til presisjonsparameterene til hver av dem, henholdsvis $\tau_u$ og $\tau_r$,  slik at $\log(\tau_u) \sim \text{Gamma}(1, 5\times 10^{-5})$ og tilsvarende $\log(\tau_r) \sim \text{Gamma}(1, 5\times 10^{-5})$. For mer informasjon om disse fordelingene og parameteriseringene som er brukt i INLA, kan man skrive inn `inla.doc("besag")` og `inla.doc("loggamma")` i R etter man har lastet inn INLA.

Etter denne modellen blir tilpasset i INLA på alle trafikklenkene, hentes median-verdien til à posteriori fordelingen for hver trafikklenke ut og angis som punkt-estimat for den trafikklenka, mens standardavviket brukes som mål på usikkerheten til estimatet.

#### **Balansering**
For å sikre at trafikkmenden forblir balansert mellom inngående og utgående trafikk fra et kryss, introduseres et tilleggsteg etter manglende ÅDT har blitt estimert i INLA. Denne balanseringen ble først beskrevet av [Haug et al. (2021)](https://nr.no/publikasjon/1962532/), og tilleggskriterier ble introdusert i [Haug og Aldrin (2022)](https://nr.no/publikasjon/2070401/). To av kravene presentert i [Haug og Aldrin (2022)](https://nr.no/publikasjon/2070401/) ble ikke brukt, med deres notasjon ble kravene i matrise $A_4$ (total predikert ÅDT summert over alle trafikklenker er lik summen av balansert ÅDT) og $A_5$ (balansering av motgående trafikk) utelatt. Det står da igjen fire ulike typer krav: 1) balansering av trafikken i kryss, 2) målefeil på preliminær ÅDT fra trafikkregistreringspunkter, 3) total målt ÅDT summert over lenker med trafikkregistreringspunkter, og 4) svingebevegelser i kryss.

***Notasjon for balanseringa***

- $n_n$ - antall trafikknoder.
- $n_e$ - antall trafikklenker (retta).
- $n_p$ - antall trafikklenker med trafikkregistrering.
- $n_s^I$ - samlet antall inngående trafikklenker til alle kryss i vegnettsgrafen ($n_s^I \leq n_e$).
- $\boldsymbol{v} = (v_{ij})$ for $i,j = 1,\dots, n_n$ - vektor med de sanne ÅDT-verdiene for alle trafikklenker. Subscriptene indikerer henholdsvis start- og sluttnode for den gitte lenka.
- $\boldsymbol{\mu}_v$, $\boldsymbol{\Sigma}_v$ - predikert ÅDT og tilhørende usikkerhet fra modell-steget.
- $\boldsymbol{d}$, $\boldsymbol{\Sigma}_\varepsilon'$ - målt ÅDT og tilhørende usikkerhet. 

**1. Balansering av trafikken i kryss**
Trafikken inn og ut av et kryss skal være lik. Dette kravet gjaldt også i [Haug et al. (2021)](https://nr.no/publikasjon/1962532/). Vi definerer ei retta insidensmatrise $\boldsymbol{A}_1$ som angir hvilke trafikklenker er knyttet til de ulike kryssene. $\boldsymbol{A}_1$ har dimensjon $n_n\times n_e$. For rad $i$ i matrisa er alle verdiene 0 bortsett fra de kolonnene som tilsvarer trafikklenker som går ut fra (verdi = 1) eller inn mot (verdi = -1) trafikknode $i$. Hver trafikklenke (representert av ei kolonne) vil kun ha to verdier som ikke er null, nemlig noden (representert av en rad) hvor lenka starter (verdi = 1) og noden hvor den slutter (verdi = -1). Når matrisa $\boldsymbol{A}_1$ er konstruert slik, kan dette kravet skrives som

$$
\boldsymbol{A}_1\boldsymbol{v} = \boldsymbol{0} \ .
$$

**2. Målefeil på preliminær ÅDT fra trafikkregistreringspunkter**
Alle trafikkmengdene fra trafikkregistreringspunkt er målt med varierende grad av målefeil. Vi definerer ei matrise $\boldsymbol{A}\_2$ som har dimensjon $n_p\times n_e$, og som skal indikere om ei gitt trafikklenke har trafikkregistrering. Vi ender opp med ei matrise hvor hver rad har bare 0 bortsett fra ett elemnet som er 1 i den kolonna som korresponderer til den trafikklenka som er i tilsvarende rad. Det betyr at hver rad har bare ett element som har verdi 1. Noen kolonner har kun nuller. Målefeilsmodellen blir da

$$
\boldsymbol{A}_2\boldsymbol{v} + \boldsymbol{\varepsilon'} = \boldsymbol{d} \ ,
$$

hvor $\boldsymbol{d}$ er en $n_p\times 1$-vektor med de registrerte tallene, og $\boldsymbol{\varepsilon}' \sim \mathcal{N}(\boldsymbol{0}, \boldsymbol{\Sigma}\_\varepsilon')$, hvor $\boldsymbol{\Sigma}_\varepsilon'$ er ei diagonal matrise med elementer svarende til variansen til de preliminære ÅDT-tallene. 

**3. Total målt ÅDT summert over lenker med trafikkregistreringspunkter**
Dette kriteriet sier at summen av alle de balanserte ÅDT-estimatene på steder hvor vi har registreringapunkter er lik summen av de målte verdiene på disse punktene. Vi innfører radmatrisa $\boldsymbol{A}\_3$, som har dimensjon $1\times n_e$, hvor raden i $\boldsymbol{A}\_3$ har verdi $1$ i kolonnene som svarer til lenker med trafikkregistreringspunkter og $0$ ellers. Vi får

$$
\boldsymbol{A}\_3\boldsymbol{v} + \Sigma_{k = 1}^{n_p}\varepsilon_k' = \Sigma_{k = 1}^{n_p}d_k \ .
$$


**4. Svingebevegelser i kryss**
I dette steget legger man flere betingelser på trafikkflyten gjennom kryss, slik at man unngår at balanseringa i steg 1 bare fører til at all trafikken snur og kjører tilbake i kryss. Spesifikt defineres det to sett med koeffisienter, der det ene settet knyttes til hvordan trafikken på ei gitt lenke inn i et kryss fordeler seg utover på de utgående lenkene, mens det andre settet knyttes til hvordan trafikken på ei gitt lenke ut av krysset har kommet inn i krysset. 

*4.1 Fordeling av inngående trafikk*
[Haug og Aldrin (2022)](https://nr.no/publikasjon/2070401/) skriver 

> "Koeffisientene $\omega_{li}^{(jl)}$ fordeler $v_{jl}$ proporsjonalt på de ulike lenkene ut fra kryss $l$ ved for hver lenke $li$ å skalere $v_{jl}$ mot den samlede trafikken langs alle lenkene inn mot kryss $l$ som potensielt kan gi bidrag til $v_{li}$. I tillegg normaliseres koeffisientene med totaltrafikken ($\Sigma_i v_{il}$) inn mot krysset."

Koeffisientene for trafikken som kommer inn på lenke $jl$ uttrykkes da ved 

$$
\omega_{li}^{(jl)} = \frac{v_{jl}}{\Sigma_{k\neq i}v_{kl} \cdot \Sigma_i v_{il}} \ ,
$$

og det vil være en slik koeffisient for hver utgående trafikklenke fra kryss $l$.

På matriseform kan vi skrive 

$$
\boldsymbol{A}_\omega\boldsymbol{v} = \boldsymbol{0} \ ,
$$

hvor $\boldsymbol{A}\_\omega$ har dimensjon $n_s^I\times n_e$. Det betyr at for hvert kryss så er det så mange rader som krysset har inngående trafikklenker, og hver kolonne tilsvarer trafikklenkene. En gitt rad i $\boldsymbol{A}\_\omega$ har verdi $-1$ i elementet som tilsvarer den aktuelle inngående trafikklenka $jl$, og koeffisientene $\omega_{li}^{(jl)}$ for de respektive utgående lenkene $li$ som leder trafikken $v_{jl}$ ut fra kryss $l$. Alle andre elementer i raden har verdi 0.

[Haug og Aldrin (2022)](https://nr.no/publikasjon/2070401/) beskriver hvordan betingelsen over er mer streng enn vi ønsker. Dette er knyttet et par ulike momenter. For det første forhindrer betingelsen trafikken fra å returnere samme vei som den kom fra, noe som ikke alltid er rimelig, særlig i rundkjøringer. I tillegg er det en veldig streng betingelse. Til slutt uttrykkes koeffisientene i matrisa $\boldsymbol{A}\_\omega$ ved den sanne trafikken $\boldsymbol{v}$, som vi ikke har tilgang til i praksis. I stedet må vi erstatte $\boldsymbol{v}$ med kjente estimater $\hat{\boldsymbol{v}}$, slik at vi får $\widehat{\boldsymbol{A}}\_\omega = \boldsymbol{A}\_\omega(\hat{\boldsymbol{v}})$ . Vi innfører også feilleddene $\boldsymbol{\varepsilon}_s^I = (\varepsilon_s^{(jl),I})$, slik at vi får

$$
\widehat{\boldsymbol{A}}_\omega \boldsymbol{v} + \boldsymbol{\varepsilon}_s^I = \boldsymbol{0} \ ,
$$

hvor $\boldsymbol{\varepsilon}\_s^I \sim \mathcal{N}(\boldsymbol{0}, \boldsymbol{\Sigma}\_\varepsilon^{s,I})$, med $\boldsymbol{\Sigma}\_\varepsilon^{s,I} = \text{diag}((\sigma_\varepsilon^{s,I})^2)$, der $\sigma_\varepsilon^{s,I}$ beskriver usikkerheten i de inngående svingelikningene. Vi beskriver hvilke verdier denne usikkerheten settes til etter vi har beskrevet koeffisientene for den utgående trafikken. 

*4.2 Kilder til utgående trafikk*
Helt tilsvarende har vi for en utgående trafikklenke $lj$ koeffisientene 

$$
\tau_{il}^{(lj)} = \frac{v_{lj}}{\Sigma_{k\neq i}v_{lk}\cdot \Sigma_iv_{li}} \ ,
$$

og vi får en slik koeffisient for hver inngående trafikklenke til kryss $l$. Vi strukturerer koeffisientene i matrisa $\boldsymbol{A}_\tau$, slik at vi får 

$$
\boldsymbol{A}_\tau\boldsymbol{v} = \boldsymbol{0} \ ,
$$

hvor $\boldsymbol{A}\_\tau$ har dimensjon $n_s^U\times n_e$, altså det vil for hvert kryss være så mange rader som det krysset har utgående lenker. For en gitt rad i matrisa vil den ha verdi $-1$ i elementet som tilsvarer den aktuelle utgående trafikklenka $lj$, og koeffisientverdiene $\tau_{il}^{(lj)}$ for de respektive inngående lenkene $il$ som bidrar til trafikken $v_{lj}$. De andre elementene er lik null. Vi innfører som over feilleddene $\boldsymbol{\varepsilon}\_s^U = (\varepsilon_s^{(jl),U})$ og bruker ei estimert matrise $\widehat{\boldsymbol{A}}\_\tau = \boldsymbol{A}_\tau(\hat{\boldsymbol{v}})$. Vi har da

$$
\widehat{\boldsymbol{A}}_\tau \boldsymbol{v} + \boldsymbol{\varepsilon}_s^U = \boldsymbol{0} \ ,
$$

hvor $\boldsymbol{\varepsilon}\_s^U \sim \mathcal{N}(\boldsymbol{0}, \boldsymbol{\Sigma}\_\varepsilon^{s,U})$, med $\boldsymbol{\Sigma}\_\varepsilon^{s,U} = \text{diag}((\sigma_\varepsilon^{s,U})^2)$, der $\sigma_\varepsilon^{s,U}$ beskriver usikkerheten i de utgående svingelikningene.

*4.3 Proposjonalitetskonstanter*
Denne usikkerheten setter vi til ulike verdier avhengig av hvilket kryss vi befinner oss i. For I-kryssene har vi en fast verdi $\sigma_\varepsilon^{s,I} = \sigma_\varepsilon^{s,U} = 0.01$, mens vi for andre kryss uttrykker usikkerheten ved hjelp av residualene til de inngående og utgående svingeligningene beregnet over alle kryss som ikke er I-kryss, henholdsvis $\boldsymbol{r}\_\varepsilon^I$ og $\boldsymbol{r}\_\varepsilon^U$, slik at $\sigma_\varepsilon^{s,\cdot} = c_\varepsilon^s = (\overline{(\boldsymbol{r}\_\varepsilon^I)^2 + (\boldsymbol{r}_\varepsilon^U)^2})^{1/2}$.

**Setter det sammen**
Vi definerer nå 

$$
\boldsymbol{A} = \begin{bmatrix}
\boldsymbol{A}\_1\\
\boldsymbol{A}\_2\\
\boldsymbol{A}\_3\\
\boldsymbol{A}\_\omega\\
\boldsymbol{A}\_\tau\\
\end{bmatrix} \ , \quad
\boldsymbol{\varepsilon} = \begin{bmatrix}
\boldsymbol{0}\\
\boldsymbol{\varepsilon}'\\
\Sigma_{k = 1}^{n_p}\varepsilon_k'\\
\boldsymbol{\varepsilon}\_s^I\\
\boldsymbol{\varepsilon}\_s^U\\
\end{bmatrix} \quad \text{og} \quad
\boldsymbol{b} = \begin{bmatrix}
\boldsymbol{0}\\
\boldsymbol{d}\\
\Sigma_{k = 1}^{n_p}d_k\\
\boldsymbol{0}\\
\boldsymbol{0}\\
\end{bmatrix} \ .
$$

Dette gir oss uttrykket 

$$
\boldsymbol{A}\boldsymbol{v} + \boldsymbol{\varepsilon} = \boldsymbol{b} \ . \qquad \text{(1)}
$$

Her antar vi en à priori-fordeling for $\boldsymbol{v}$ som

$$
\boldsymbol{v} \sim \mathcal{N}(\boldsymbol{\mu}_v, \boldsymbol{\Sigma}_v) \ , \qquad \text{(2)}
$$

hvor $\boldsymbol{\mu}_v$ er de predikerte ÅDT-estimatene (fra modellen), og $\boldsymbol{\Sigma}_v$ er usikkerheten i prediksjonene, begge disse kommer fra modellen.

For feilleddet $\boldsymbol{\varepsilon}$ i $(1)$ bruker vi

$$
\boldsymbol{\varepsilon} \sim \mathcal{N}(\boldsymbol{0}, \boldsymbol{\Sigma}_\varepsilon) \ , \qquad \text{(3)}
$$

hvor

$$
\qquad \boldsymbol{\Sigma}\_\varepsilon = 
\begin{bmatrix}
0 & 0 & 0 & 0 & 0 \\
0 & \boldsymbol{\Sigma}\_\varepsilon' & 0 & 0 & 0 \\
0 & 0 & \Sigma_{k = 1}^{n_p}\text{diag}(\boldsymbol{\Sigma}\_\varepsilon')_k & 0 & 0 \\
0 & 0 & 0 & \boldsymbol{\Sigma}\_\varepsilon^{s,I} & 0 \\
0 & 0 & 0 & 0 & \boldsymbol{\Sigma}\_\varepsilon^{s,U} \\
\end{bmatrix}
$$

Fra à priori-fordelingen $(2)$ og ligningssystemet $(1)$ følger det at 

$$
\boldsymbol{b} \sim \mathcal{N}(\boldsymbol{A}\boldsymbol{\mu}_v, \boldsymbol{\Sigma}_b) \ ,
$$

hvor $\boldsymbol{\Sigma}\_b = \boldsymbol{A} \boldsymbol{\Sigma}\_v\boldsymbol{A}^\top + \boldsymbol{\Sigma}_\varepsilon$. Videre følger det da at simultanfordelingen til to normalfordelte stokastiske variabler er

$$
\begin{bmatrix}
\boldsymbol{v} \\
\boldsymbol{b}
\end{bmatrix}
\sim \mathcal{N}\Biggl(
\begin{bmatrix}
\boldsymbol{\mu}\_v \\
\boldsymbol{A}\boldsymbol{\mu}\_v
\end{bmatrix} ,
\begin{bmatrix}
\boldsymbol{\Sigma}\_v & \boldsymbol{\Sigma}\_{vb} \\
\boldsymbol{\Sigma}\_{bv} & \boldsymbol{\Sigma}\_b
\end{bmatrix}
\Biggr) \ ,
$$

hvor $\boldsymbol{\Sigma}_{vb} = \boldsymbol{\Sigma}\_{bv}^\top$ er kovariansmatrisa mellom $\boldsymbol{v}$ og $\boldsymbol{b}$. Fra $(1)$ følger det at

$$
\boldsymbol{\Sigma}_{vb} = \boldsymbol{\Sigma}_v\boldsymbol{A}^\top \ ,
$$

siden vi antar at den sanne ÅDT-verdien $\boldsymbol{v}$ er uavhengig av målefeilen $\boldsymbol{\varepsilon}$. Fra dette kan vi finne à posteriori-fordelingen til $\boldsymbol{v}$, som er den betingede fordelingen for $\boldsymbol{v}$ gitt dataene $\boldsymbol{b}$,

$$
\boldsymbol{v}\mid\boldsymbol{b} \sim \mathcal{N}(\boldsymbol{\mu}\_{v\mid b}, \boldsymbol{\Sigma}_{v\mid b}) \ ,
$$

hvor

$$
\begin{aligned}
\boldsymbol{\mu}\_{v\mid b} & = \boldsymbol{\mu}\_v + \boldsymbol{\Sigma}\_{vb}\boldsymbol{\Sigma}\_{b}^{-1}(\boldsymbol{b} - \boldsymbol{A\mu}\_v) \\
\boldsymbol{\Sigma}\_{v\mid b} & = \boldsymbol{\Sigma}\_{v} - \boldsymbol{\Sigma}\_{vb}\boldsymbol{\Sigma}_{b}^{-1}\boldsymbol{\Sigma}\_{vb}^\top
\end{aligned} \ . \tag{4}
$$

Ligningssystemet i $(1)$ kan løses iterativt, ved å for hver iterasjon estimere $\widehat{\boldsymbol{A}}\_{\omega}$ og $\widehat{\boldsymbol{A}}\_{\tau}$ på nytt, og så ved hjelp av det lage nye estimater $\hat{\boldsymbol{v}}$. Deretter oppdateres $\widehat{\boldsymbol{A}}\_{\omega}$ og $\widehat{\boldsymbol{A}}\_{\tau}$ på nytt utifra det siste estimatet for $\hat{\boldsymbol{v}}$, og slik fortsetter man i et angitt antall iterasjoner. Om algoritmen konvergerer, så vil $\boldsymbol{\mu}\_{v\mid b}$ og $\boldsymbol{\Sigma}_{v\mid b}$ stabilisere seg på det som vi da kaller de balanserte ÅDT-verdiene, og tilhørende usikkerhet, for alle trafikklenkene. Algoritmen er spesifisert med pseudokode under (fra [Haug og Aldrin, 2022](https://nr.no/publikasjon/2070401/) )

![[algoritme_aadt.png]]
#### **Fra retta til uretta trafikklenker**
Etter balanserte ÅDT-prediksjoner $\hat{\boldsymbol{v}}$ har blitt regnet ut på alle de retta trafikklenkene, så aggregeres disse opp til de uretta lenkene ved å knytte dem til foreldre-id'en til de retta lenkene. For trafikklenker med enveiskjøring vil det være samme verdi på retta som på uretta lenke, mens der det er toveiskjøring vil trafikkmengden på den uretta lenka være summen av trafikkmengden på de to retta lenkene. 

#### **Autogodkjenning**
Før de balanserte ÅDT-estimatene ble sendt til manuell kontroll, ble de kjørt gjennom en autogodkjenningsalgoritme slik at hvert estimat, i tillegg til usikkerhet, fikk en autogodkjent-status (godkjent/ikke godkjent). Fagpersonene som korrigerte verdiene kunne dermed fokusere ekstra på de som ikke var autogodkjent. 

Autogodkjenninga baserer seg på både eALE og den predikerte usikkerheten. eALE er en funksjon av den predikerte trafikkmengden for ei gitt trafikklenke, $\hat{v}_i$, og det man bruker som sammenligningsgrunnlag, i dette tilfellet var det trafikkmengden fra i fjor, $v_i$. Vi regner ut eALE som

$$
eALE(v_i, \hat{v}_i) = \exp\{|\log(\hat v_i)-\log(v_i)|\} - 1 \ .
$$

Så definerer vi en terskelfunksjon basert på predikert ÅDT, som sier hvilke eALE vi aksepterer, altså, hvor store avvik vi aksepterer for en predikert trafikkmengde. Denne terskelfunksjonen er definert som 

$$
t(\hat{v}) = 
\begin{cases}
2-\frac{1.6}{1+\exp\{-0.01(\hat{v}_i-500)\}} & \text{if } 0 \leq \hat{v}_i \leq 1000 \ , \\
0.41055 - 0.15(\frac{\hat{v}_i-1000}{9000})^{0.5} & \text{if } 1000 < \hat{v}_i \leq 10000 \ , \\
0.26055 - 0.06(\frac{\hat{v}_i-10000}{40000})^{0.5} & \text{if } 10000 < \hat{v}_i \leq 50000 \ , \\
0.2 & \text{if } \hat{v}_i \geq 50000 \ .
\end{cases}
$$

I tillegg vurderes prediksjonen sammen med predikert usikkerhet. For sammenligningsverdien $v_i$, konstrueres en fiktiv usikkerhet basert på trafikkmengdens størrelse. Helt konkret settes denne som

$$
s(v_i) = 
\begin{cases} 
40 & \text{if } v_i < 500 \\
100 & \text{if } 500 \leq v_i < 1000 \\
200 & \text{if } 1000 \leq v_i < 5000 \\
400 & \text{if } 5000 \leq v_i < 10000 \\
1000 & \text{if } 10000 \leq v_i < 30000 \\
2000 & \text{if } v_i > 30000 \ .
\end{cases}
$$

En predikert verdi kan da også forkastes dersom det ikke er noe overlapp i konfidensintervallene som resulterer fra usikkerheten regnet fra $s(v_i)$ og den predikerte usikkerheten til $\hat{v}_i$. 

Samlet sett sier altså autogodkjenningsalgoritmen at en prediksjon autogodkjennes dersom $eALE(v_i, \hat{v}_i) < t(\hat{v}_i)$ og det er overlapp i konfidensintervallene til $v_i$ og $\hat{v}_i$.

## Tungbilandel-modellen
Siden tungbil-andelen $t_i$ er et tall som varierer mellom 0 og 1, ble det brukt en beta-regresjon til å modellere denne andelen. Mer presist kan modellen som ble brukt beskrives som

$$
\begin{aligned}
t_i &~ \sim \text{Beta}(\mu_i, \phi) \ , \\
\text{logit}(\mu_i) &= \beta_0 + \beta_1x_1 + \beta_2x_2 + \dots + \beta_px_p \ ,
\end{aligned}
$$

hvor $x_1, \dots, x_p$ er forklaringsvariablene som ble brukt i modellen. For denne modellen ble det valgt ut forklaringsvariabler på tilsvarende vis som for ÅDT-modellen. Formelen som ble brukt var:
```
formula_heavy <- heavyRatio ~ 
    functionalRoadClass * maxLanes + 
    minLanes * highestSpeedLimit +
    minLanes * functionClass +
    roadCategory * numberOfEstablishments + 
    functionClass:numberOfEmployees + 
    roadCategory
```

For koeffisientene $\beta_1, \dots, \beta_p$ ble det brukt default à priori-fordelingene i INLA, altså $\mathcal{N}(0, 0.001)$, og skjæringspunktet $\beta_0$ fikk fordeling $\mathcal{N}(0, 0)$.

# *Resultater*

Etter modellen hadde estimert ÅDT på alle trafikklenkene uten registreringer, ble disse estimatene tilgjengelig for de lokale fagpersonene for manuell korrigering. De fikk da se de modellpredikerte verdiene, og fikk endre på disse ved behov. Det betyr at vi til slutt sitter igjen med både modellpredikerte og manuelt endra verdier, og kan sammenligne disse.

*54% av alle trafikklenker ble ikke endra, noe som utgjør 67% av km på vegnettet.*

Det er ikke helt samsvar mellom de trafikklenkene som ble autogodkjent og de som forble uendra, se tabell under. Vi ser at av de 15025 trafikklenkene som ble autogodkjent, så ble 3249 av de manuelt endra, det utgjør ca 22% av de autogodkjente trafikklenkene. Av de 7950 trafikklenkene som ikke ble autogodkjent, så ble likevel 1587 manuelt godkjent, som tilsvarer 20% av de ikke autogodkjente trafikklenkene.

|                       | **Manuelt endra** | **Manuelt godkjent** |
| --------------------- | ----------------- | -------------------- |
| **Ikke autogodkjent** | 6363              | 1587                 |
| **Autogodkjent**      | 3249              | 11776                |

Hvis vi ser på punktplottet som viser de modellpredikerte verdiene mot de korrigerte verdiene, så ser vi at det er en del ekstreme overestimeringer. Disse finner vi hovedsaklig i flerplanskryss, ofte i Oslo-området.
![[korr_vs_modell_rapport.png]]

I tillegg har vi i ettertid snublet over modellpredikerte verdier som skulle vært korrigert, men som ble manuelt godkjent. Derfor er det viktig å være bevisst på at de manuellt korrigerte verdiene ikke kan sees på som en fasit. I bildet under vises krysset over omkjøringsvegen (E6) ved Voll i Trondheim. Her er det nok registreringer til å utlede en sikker verdi selv på de to rampene på sør-øst-siden av E6. De ble estimert av modellen til 8177, når det skulle vært 3027, og 10895, når det skulle vært 5965.
![](moholt_e6_kryss.jpg)

## Registreringer av trafikkmengder
**Kontinuerlige målinger**
På steder med kontinuerlige målinger ble det oppdaget hovedsakelig to typer feil, den første var at det var støy på sensoren som førte til svært ekstreme verdier, se Kristiansund (70361V249595) og Lofoten (53019V1201959). Disse punktene ble oppdaget da de hadde rundt ti ganger så høy ÅDT som kunne forventes på slike steder, og var blitt korrigert etter det. Likevel er det rart at slike steder ikke har blitt oppdaget tidligere.

Det andre tilfellet som dukket opp var at verdier som ikke er synlig på trafikkdata.no ble inkludert i datagrunnlaget til modellen. Ofte var det da svært lave verdier. Et eksempel er Hansjordnestunellen i Tromsø (80998V1125915), som var stengt i hele 2024, men hvor det likevel ble registrert 52 kjøretøy. Denne verdien ble korrigert til 9800, som var fjorårets ÅDT. Et annet eksempel er ei rampe i Romerike (53771V1204217), hvor det står 26 i Kibana, men 1174 i portalen. I det siste tilfellet er det ikke klart hva som har skjedd.

Dette tvinger også fram spørsmålet om hvordan ÅDT skal rapporteres på steder hvor veien er delvis stengt. I eksempelet i Hansjordnestunellen i Tromsø er det tydelig at reell ÅDT for 2024 var 0, da den var stengt hele året. Likevel har den blitt korrigert til 9800, som var fjorårets verdi. Et annet eksempel er Trollstigen, som alltid er vinterstengt, men som i 2024 også ble stengt etter juni pga rasfare. Der kjørte det 58 biler på 5 dager i mai (11.6 i snitt per dag) og 12807 biler på 26 dager i juni (493 per dag), dvs 415 biler per dag i snitt de dagene vegen var åpen. Hva slags ÅDT ønsker vi å representere på slike steder? 

**Periodiske målinger**
Det er en god del tilfeller hvor periodiske målinger ser litt rare ut, og hvor målte periodiske tall har blitt korrigert. Et eksempel er ved Hamar (81531V704884), hvor det kun ble målt i ei uke i 2024, og da ble det målt 73 biler på tirsdag, og 5 eller færre resten av uka. Et annet eksempel er en periodisk måler ved Austbø (23025V521410), hvor det som regel er over 1000 kjøretøy på de dagene det er gjort registreringer, men svært mange dager er registrert med 0. Det gir en ÅDT på 167, selv om det trolig skal ligge på litt over 1000. 

Et siste eksempel er Halsøbrua øst i Stjørdal (12466V2265164), hvor det ble brukt data fra en periodisk måler i 2023. I tiden den var oppe målte den konsekvent rundt 8000-10000 kjøretøy per dag, og gav en ÅDT på 8559, som ser riktig ut. Likevel ble ÅDT korrigert til 1416. Har det skjedd en så stor endring i trafikkmengden? Det er kanskje risikabelt å bruke gamle periodiske målere. Tilsvarende tilfelle sees utenfor Larvik (04941V2014231), her fins det data fra 2023 som ikke åpenbart er feil, likevel har ÅDT for 2024 blitt satt til et veldig annet tall.

**Bomstasjoner**
Det ser ut til å være en del steder hvor bomstasjoner har blitt plassert feil. Et eksempel er ved Alnabru (0.03032036-0.12081584@625215, EV6 S17D1 m262-938), hvor en bomstasjon som kun målte trafikken i ett felt har blitt plassert på begge feltene, slik at trafikken blir kraftig underestimert. Tilsvarende feil har skjedd i en tunell ved Sinsen (0.0@2312223-0.31303493@625405, RV150 S1D1 m3561-4270). Flere av feilene av denne typen ligger i Oslo.

I tillegg ble bomstasjoner prioritert over egne målinger når begge disse var tilgjengelige på samme sted. Det fører til litt rare tall når bomstasjonen også kan ha blitt plassert feil, som for eksempel på Madlaveien i Stavanger (55507V319881).

**Utleda verdier** 
Bruk av utleda verdier kan introdusere feil eller usikkerhet, men fordi bruken av utleda verdier kontrolleres ganske nøye, er dette trolig ikke et problem. Tvert i mot ser det heller ut som det er steder hvor verdier kunne blitt utleda, men ikke har blitt det. Eksempler på det fins for eksempel på Ålesundvegen inn mot Ålesund.

**Data på tungbiler** 
Det ble ikke brukt data på utleda tungbil-andeler. Det er usikkert om det er hensiktsmessig å inkludere denne dataen, siden disse mindre tallene er mer utsatt for målefeil. Likevel kan det trolig være lurt å inkludere utleda tungbilantall dersom man går over til å modellere antall i stedet for andel.

**Andre datakilder?**
Vi ser også at det kan være mye å tjene på å inkludere flere datakilder. Vi ser hovedsakling to store muligheter her: holdeplasspasseringer og floating car data.

Holdeplasspasseringer tror vi vil være særlig nyttig å bruke på steder hvor det kun finnes kollektivfelt, altså hvor buss-trafikken utgjør opp mot 100% av passeringene (bortsett fra taxier). Denne dataen er lett tilgjengelig fra EnTur's datakatalog. 

Floating car-data gir oss bare en andel av trafikken (de bilene som har en slik GPS installert), men vil likevel gi nyttig informasjon om bevegelsesmønster og steder hvor vi har lite data. 

## Trafikklenkegrafen
Trafikklenkegrafen er basert kun på Europaveier, riksveier, fylkesveier, og utvalgte viktige kommunale veier. Den inneholder ikke informasjon om avkjøringer til viktige områder (boligfelt, kjøpesenter) på private eller kommunale veier. Dette vil bidra til feil når grafen brukes til balansering av tallene. I tillegg mangler potensielt viktige forbindelser, f.eks. Jonsvannsveien over Moholt. 

## ÅDT-modellen

#### **Den statistiske modellen**
*Modellklassen* 
Det ble brukt en Poisson-modell, men det kan være verdt å sammenligne med en gamma eller negativ binomisk modell i stedet, grunnet stor overspredning i ÅDT-tallene. 

*Forklaringsvariabler*
Det kan være relevante forklaringsvariabler vi ikke har tatt med, for eksempel informasjon om hvor det kun er kollektivtrafikk. I tillegg kan det finnes målefeil og andre feilkilder i forklaringsvariablene.

*Den romlige komponenten*
Vi brukte en Besag-komponent, som baserer seg på at verdien på ei lenke er normalfordelt med forventningsverdi lik snittet av nabo-lenkene. Det gir mening for en landevei, men ikke så mye mer enn det. Et eksempel på et sted hvor dette kom tydelig fram var langs Ålesundveien, hvor det fantes svært mange målinger men trafikkmengden på hovedveien ble underestimert på grunn av lavere trafikkmengder på rampene som grensa til den.

*Modellseleksjon og evaluering*
Kryssvalideringen foregikk som beskrevet ved å velge ut tilfeldige trafikklenker. Fordi trafikklenkenettverket har en underliggende romlig struktur med romlige korrelasjoner, kan det være lurt å heller bruke en form for romlig blokking når man velger ut "foldene" til kryssvalideringa. Det er alternativt mulig å gruppere kryssvalideringa basert på andre karakteristikker som for eksempel vegkategori, eller noe annet. Konsekvensen av å bruke kryssvalidering på data som har en slik korrelasjonsstruktur er at man risikerer å overtilpasse modellen til trenings-dataen, og dermed ender opp med å velge en for kompleks modell, og også får overoptimistiske ytelsesmetrikker. 

Modellen er svak på vegstrukturer hvor vi ikke har mye trenings-data. Eksempler er av- og på-kjøringsfelt fra en hovedvei med en del trafikk, til en mindre vei som ikke er med i ÅDT-belegginga. Her skal som regel trafikkmengden være lavere, men den blir fordelt jevnt over hoved-veien og av/på-kjøringsfeltene. Et annet sted hvor modellen sliter på grunn av mangel på data er steder hvor det kun er kollektivtrafikk. En forutsetning for å forbedre disse stedene er at vi har en indikatorvariabel som kommuniserer at vi befinner oss på et slikt sted, samt at vi har noe treningsdata der. For steder med kun kollektivtrafikk har vi planer for å få på plass begge, men for av/på-kjøringsfeltene har vi ingen planlagte løsninger. 

For bestemte identifiserte strukturer som har en karakteristisk trafikkmengde, men hvor vi mangler treningsdata, så kan en løsning være å inkludere en indikatorvariabel for den strukturen i modellen, og så å sette en informativ à priori fordeling på den respektive regresjonskoeffisienten for å "påtvinge" det vi vet om hvordan en slik struktur påvirker trafikkmengden. For både nebb og kollektiv-lenker vet vi at de har en lavere trafikkmengde enn andre strekninger, så da kan vi sette en à priori fordeling som trekker den predikerte mengden nedover.

#### **Balansering** 
Balanseringa introduserer ekstremverdier som ikke stemmer. Dette skjer spesielt i flerplanskryss. Det er uklart nøyaktiv hva som er grunnen til at balanseringa slår så feil ut akkurat på slike steder. Vi vet også at resultatene fra INLA noen ganger ser rare ut på slike steder, men balanseringa innfører ofte langt mer alvorlige feil. 

I tillegg til denne innføringa av ekstremt høye verdier, så ser vi også mange steder som ikke blir balansert når de skulle vært det, så det ser ut som balanseringa slår inn veldig selektivt. Det er uklart hvorfor begge disse feilene skjer. 

Et annet aspekt er at balanseringa ikke tar hensyn til steder hvor det kun er tillatt med kollektivtrafikk. Dette er likevel trolig ikke et problem, da balanseringa idéelt sett bare skal introdusere små justeringer.

## Tungbilandel-modellen

#### **Den statistiske modellen**
*Modelltype*
Det ble brukt en beta-modell og andelen ble modellert direkte. Hvis vi skal bruke en romlig komponent gir det mening å gå over til å modellere antall direkte.

*Forklaringsvariabler*
Det bør inkluderes informasjon om hvor det bare er kollektivtrafikk, på steder med kun kollektivtrafikk vil tungbil*andelen* være svært høy. Tungbil*antallet* vil derimot ikke nødvendigvis være høyere enn på andre steder, men den vil kanskje ikke være så mye lavere, eller, forskjellen vil ihvertfall ikke være like dramatisk som for total-trafikken.

*Den romlige komponenten*
Det ble ikke bruk en romlig komponent, men det vil gi mening å inkludere en romlig komponent om vi modellerer antall direkte.

#### **Balansering**
Tungbil-andelen ble ikke balansert, da det ikke gir mening å balansere andelen direkte. Men hvis vi bruker antall i stedet, vil det gi mening å balansere disse tallene, dersom det fremdeles er nødvendig etter en romlig modellering.


![[ådt2024modell_skisse.jpg.jpg]]



# *Oppsummering av viktige forbedringer*

Til ÅDT-belegginga i 2025 er det et par viktige endringer som skal gjøres. Mange av disse krever betydelige undersøkelser før de kan tas i bruk, og noen preliminære steg er da beskrevet.

**Nye datakilder**
Vi ønsker å inkludere informasjon om antall buss-passeringer på strekninger hvor det kun er kollektiv-felt. EnTur har god data på dette og det vil ikke være for vanskelig å inkludere på de stedene hvor det ligger et busstopp direkte på ei lenke med bare kollektivfelt. I tillegg fins det steder hvor det ligger en holdeplass like før/etter veien deler seg i egen kollektiv-lenke. Et mer komplisert steg vil være å se om man kan få beriket også disse kollektiv-lenkene med data fra holdeplasser i nærheten. 

Vi vil også begynne å utforske FloatingCar-data. Denne typen data dekker kun en andel av den totale trafikken, men kan for eksempel brukes som en forklaringsvariabel i modellen, og vil da trolig "utkonkurrere" de fleste andre forklaringsvariablene. Et viktig aspekt vil være å undersøke om andelen av biler som fanges opp av FloatingCar-data endrer seg geografisk eller basert på visse forklaringsvariabler, slik at det kan være nødvendig å inkludere en interaksjonseffekt mellom FloatingCar-antallet og eventuelle slike variabler. 

**Romlig komponent**
Vi ser svakheter ved å bruke et Besag-ledd som romlig komponent, da denne er retningsblind og ikke tar hensyn til hvor trafikk kommer inn og forsvinner ut. Vi vil undersøke muligheten for å erstatte denne komponenten med et alternativ som tar hensyn til retning.

**Modellering av tungbil*antall* i stedet for *andel***
En viktig endring vil være at vi endrer den statistiske modellen for tungbiler til å predikere antall tungbiler i stedet for andelen. Det innebærer at vi bytter rimelighetsfunksjon til gamma eller Poisson, og medfører også at det kan gi mening å innføre en romlig komponent også i denne modellen. Strukturen på tungbilmodellen vil da være ganske lik den generelle ÅDT-modellen, men det kan være relevant å inkludere andre forklaringsvariabler.

**Svakhet i utleda tungbilantall**
Vi må ta stilling til om vi ønsker å bruke utleda tungbil-antall. Fordi vi kommer på et nivå hvor usikkerhetene vil være større, kan det hende at disse tallene vil være misvisende. Det kan også hende at dette er noe som må tas tak i på måle-siden, for å få mer presise tall i utgangspunktet. Det gjelder både totale trafikktall og tungbil-antall, siden begge disse spiller inn i tungbil-andelen, som vi ønsker å rapportere ut.

**Hva betyr ÅDT?**
Vi må ta stilling til hva ÅDT egentlig skal representere, og hvilket tall som skal brukes når en vei har vært stengt. Dette vil påvirke hva slags data vi ønsker å bruke i modellen, og det kan også være relevant å inkludere data om vegstenginger på noe vis for å evaluere når det vil være riktig å bruke gammel data, og når en endring i trafikkmengde er på grunn av en faktisk "normal" endring, eller når den er på grunn av vegstenging. Vanlig praksis nå (ihvertfall noen steder) er at midlertidig stengte strekninger får satt ÅDT til fjorårets verdi, eller at de implisitt får "fyllt inn" normal trafikkmengde for perioden den var stengt. Men fordi trafikken som ikke kjørte der vil omfordeles på andre vegstrekninger, så kan dette gi et inntrykk av at den totale trafikkmengden har økt, siden de strekningene som mottar den ekstra trafikken ikke vil være mulig å identifisere automatisk. 


# *Referanser*

Haug, O., Aldrin, A., Hulleberg, N., Ingebrigtsen, R., Gjøvåg, C. (2021) *Ny metodikk for ÅDT-belegging av vegnettet* (SAMBA/52/21) Norsk Regnesentral. [Ny metodikk for ÅDT-belegging av vegnettet - NR](https://nr.no/publikasjon/1962532/)

Haug, O., Aldrin, M. (2022) *Ny metodikk for ÅDT-belegging av vegnettet - med tilleggskriterier* (SAMBA/23/22) Norsk Regnesentral. https://nr.no/publikasjon/2070401/

Statens vegvesen. (u. å.). Om dekningsgrad. I _Trafikkdata_. Hentet 25.08.2025, fra [https://trafikkdata.atlas.vegvesen.no/#/om-trafikkdata#om-dekningsgrad](https://trafikkdata.atlas.vegvesen.no/#/om-trafikkdata#om-dekningsgrad)
