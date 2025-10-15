## Hierarquia de Memória Cache
Repositorio para armazenar trabalho da cadeira de Organização e Arquitetura de Computadores.

especificacao da hierarquia:

<img width="450" height="245" alt="image" src="https://github.com/user-attachments/assets/bef884db-0b65-449d-bb8b-ec7e860833b0" />

(i) endereço - aponta para o endereço de um dado ou instrução; 
(ii) controle - habilita o acesso à memória e define se este acesso é de leitura ou escrita;
(iii) status - indica se a informação já foi lida ou escrita na/da memória;
(iv) dados ou instruções - porta bidirecional no caso da memóriaser de dados (efetua escrita e leitura), e porta unidirecional no caso da memória ser de instruções (só leitura).

Um programa que testa a hierarquia de memória deve fazer diversos acessos, forçando que ocorram casos de cache miss e cache hit de forma a explorar a localidade espacial e temporal do programa. O arquivo que contém o código executável do processador pode ser obtido com um programa assembly do MIPS sendo entrada para o montador do processador (e.g., MARS).

CACHE:
    Frequencia: mesma do processador
    Mapeamento Direto 
    Write-Through
  
8 linhas
8 palavras

descrição programas:
    1) não utiliza principios de localidade
    2) nao utiliza localidade espacial
    3) nao utiliza localidade temporal
    4) utiliza principios de localidade


O QUE PRECISA FAZER:
  HDL;
  APLICAÇÂO: 5 programas em ASSEMBLY MIPS;
  RELATÓRIO COM GRÁFICO.
