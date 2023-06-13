# Idee

**(Complex) Development Environment as Code on Every Platform**

- Komplexe DEs => mehrere (grafische) Tools & Services mit spezieller Konfiguration
    - X-Server auf Host
    - custom CA
    - spezielle JDK-Version
    - z.B. Docker Runtime, Postgres, Docker Container
    - z.B. Mailserver
    - (IJ-Lizenz)
- (minimale) DE-Config in Git / vorkonfigurierte Templates
    - Spring + Angular
    - JBOSS
    - (PAI)
    - Später: andere Sprachen und Ökosysteme
- Läuft reibungslos, im Hintergrund
- Falls nötig, Low-Level Details konfigurierbar
- /nix/store sharing => minimale VMs
- Cross platform: Windows (WSL), Linux (Podman), Später: NixOS (nixos-container), MacOS (LIMA)

