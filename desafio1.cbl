      *Divisão de identificação do programa
       Identification Division.
       Program-id. "desafio1".
       Author. "Stephani S. Zatta".
       Installation. "PC".
       Date-written. 01/07/2020.
       Date-compiled. 01/07/2020.

      *Divisão para configuração do ambiente
       environment division.

       configuration section.
           special-names. decimal-point is comma.

      *---Declaração de recursos externos
       input-output section.
       file-control.
       i-o-control.


      *---Declaração de variáveis
       data division.


      *---Variáveis de arquivos
       file section.


      *---Variáveis de trabalho
       Working-storage Section.

       01  relatorio   occurs 20.
           05 nome                                 pic x(10)
                                                   value spaces.
           05 filler                               pic x(03)
                                                   value " - ".
           05 diametro                             pic 9(03).
           05 filler                               pic x(03)
                                                   value " - ".
           05 preco                                pic 9(03)v99.
           05 filler                               pic x(03)
                                                   value " - ".
           05 preco_cm2                            pic 9(03)v99.
           05 filler                               pic x(03)
                                                   value " - ".
           05 diferenca_rel                        pic 9(03)v99.

       01  aux.
           05 nome_aux                             pic x(10)
                                                   value spaces.
           05 filler                               pic x(03)
                                                   value " - ".
           05 diametro_aux                         pic 9(03).
           05 filler                               pic x(03)
                                                   value " - ".
           05 preco_aux                            pic 9(03)v99.
           05 filler                               pic x(03)
                                                   value " - ".
           05 preco_cm2_aux                        pic 9(03)v99.
           05 filler                               pic x(03)
                                                   value " - ".
           05 diferenca_rel_aux                    pic 9(03)v99.

       77 ind                                      pic 9(02).
       77 menu                                     pic x(01).
       77 raio                                     pic 9(03)v99.
       77 area_pizza                               pic 9(03)v99.
       77 controle                                 pic x(10).
       77 delta_preco_cm2                          pic 9(03)v99.

      *---Variáveis para comunicação entre programas
       linkage section.


      *---Declaração de tela
       screen section.

      *---------- Inicio -----------------
      *Declaração do corpo do programa
       procedure division.

           perform inicializa.
           perform processamento.
           perform finaliza.

       inicializa section.
           move "S" to menu
           .
       inicializa-exit.
           exit.

      * ----------- Procesamento --------------
       processamento section.

           move 0 to ind
           perform until menu <> "S"

           display erase
           add 1 to ind

               if ind > 20 then
                   display "Voce atingiu o limite de 20 pizzas."
               else
                   display "Informe o nome da pizza: "
                   accept nome(ind)

                   display "Informe o diametro: "
                   accept diametro(ind)

                   display "Informe o preco: "
                   accept preco(ind)

               end-if

               perform preco-cm2

               display "Deseja cadastrar mais uma pizza? ('S'/'N')"
               accept menu

           end-perform

           perform odena-tabela
           perform calcula-percentual
           perform tabela

      *-- Itens da tabela ---
           perform varying ind from 1 by 1 until ind > 20 or nome(ind)
      -                                                      = space
               display relatorio(ind)

           end-perform

           .
       processamento-exit.
           exit.

      *----------- calcula o preço por cm2 --------------
       preco-cm2 section.

           compute raio = diametro(ind) / 2
           compute area_pizza = (3,14 * (raio * raio))
           compute preco_cm2(ind) = preco(ind) / area_pizza
           .
       preco-cm2-exit.
           exit.

      *---------- ordena itens ta tabela ---------------
       odena-tabela section.

           move "trocou" to controle
           perform until controle <> "trocou"

               move 1 to ind
               move "N_trocou" to controle

               perform until ind = 20
                       or nome(ind + 1) = space

                   if preco_cm2(ind) > preco_cm2(ind + 1) then
                       move relatorio(ind + 1) to aux
                       move relatorio(ind) to relatorio(ind + 1)
                       move aux to relatorio(ind)

                       move "trocou" to controle

                   end-if

                   add 1 to ind

               end-perform

           end-perform
           .
       ordena-tabela-exit.
           exit.

      *--------- calcula o percentual ---------------
       calcula-percentual section.

           move 1 to ind
           perform until ind = 20
                   or nome(ind + 1) = spaces

               compute delta_preco_cm2 = preco_cm2(ind + 1) -
      -                                  preco_cm2(ind)

               compute diferenca_rel(ind + 1) = (delta_preco_cm2 * 100)
      -                                         / preco_cm2(ind)
               add 1 to ind

           end-perform
           .
       calcula-percentual-exit.
           exit.

      *-------- "Título" de cada coluna da tabla -------------
       tabela section.

           display "Nome" at 1001
           display " "
           display "Tam" at 1014
           display " "
           display "Preco" at 1020
           display " "
           display "R$ cm2" at 1028
           display " "
           display "Diferenca" at 1036
           display " "

           .
       tabela-exit.
           exit.
      *--------- Fim -----------
       finaliza section.
          stop run.
          .
       finaliza-exit.
           exit.





