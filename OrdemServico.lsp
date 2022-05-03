;;se possivel adicionar o nome do solicitante e do executante e a quantidade de serviços

(in-package :user)

(defstruct OrdemServico
    numero
    solicitante
    executante
    )
(setq ordemservico_list (list ))
  

(defun ler_ordemservico()
    (cls)
    
    (format t "Digite o identificador da solicitante: ")
    (setq solicitante_id (read))
    (setq solicitante (nth solicitante_id solicitante_list))
        (when (not solicitante)
        (write-line "Registro não encontrado")
        (return-from ler_ordemservico 0)
    )

    (format t "Digite o identificador da executante: ")
    (setq executante_id (read))
    (setq executante (nth executante_id executante_list))
        (when (not executante)
        (write-line "Registro não encontrado")
        (return-from ler_ordemservico 0)
    )
    

    (format t "numero: ")
    (setq numero-lido (read))

    (setq os (make-OrdemServico :numero numero-lido
        :solicitante (Solicitante-documento_pessoa solicitante) :executante (Executante-documento_pessoa executante)
        ))

    (return-from ler_ordemservico os)
)

(defun cadastrar_ordemservico ()
    (cls)
    (write-line "======[Sistema OS | CAD. ORDEM DE SERVICO]======")    
    (setq so (ler_ordemservico))
    (push so ordemservico_list)
    (write-line "")
    (write-line "")    
)

(defun listar_ordemservicos ()
    (cls)
    (write-line "======[SISTEMA OS | LIST. ORDEM DE SERVICO]======")
    (write-line (concatenate 'string "Qtd. SO: " (write-to-string (list-length ordemservico_list))))
    (write-line "")
    (setq contador 0)

    (dolist (item ordemservico_list)
        (setq executante nil)
        (dolist (e executante_list)
            (if (string= (OrdemServico-executante item) (Executante-documento_pessoa e))
                (setq executante e)
            )
        )
        (setq solicitante nil)
        (dolist (s solicitante_list)
            (if (string= (OrdemServico-solicitante item) (Solicitante-documento_pessoa s))
                (setq solicitante s)
            )
        )

        (format t (concatenate 'string (write-to-string contador) " - Executante: "))
        (format t (Executante-documento_pessoa executante))
        (format t ", Solicitante: ")
        (format t (Solicitante-documento_pessoa solicitante))

        ;;mostrar a quantidades de serviços na so

        (write-line "")
        (setq contador (+ contador 1))
    )

    (write-line "")
)

(defun apagar_ordemservico()
    (cls)
    (format t "Digite o identificador do registro: ")
    (setq ordemservico_id (read))
    (setq ordemservico (nth ordemservico_id ordemservico_list))
    
    (when (not ordemservico)
        (write-line "Registro não encontrado")
        (return-from apagar_ordemservico 0)
    )


    (setq executante nil)
    (dolist (e executante_list)
        (if (string= (OrdemServico-executante ordemservico) (Executante-documento_pessoa e))
            (setq executante e)
        )
    )
    (setq solicitante nil)
    (dolist (s solicitante_list)
        (if (string= (OrdemServico-solicitante ordemservico) (Solicitante-documento_pessoa s))
            (setq solicitante s)
        )
    )

    (cls)
    (write-line "======[SISTEMA OS | APAG. ORDEM DE SERVICO]======")
    (format t (concatenate 'string "Posição: " (write-to-string ordemservico_id)))
    
    (write-line "")
    (format t "Executante: ")
    (format t (Executante-documento_pessoa executante))

    (write-line "")
    (format t "Solicitante: ")
    (format t (Solicitante-documento_pessoa solicitante))
    
    (write-line "")
    (format t "Apagar (0 = SIM | 1 = NÃO): ")
    (setq editar (read))

    (when (/= editar 0)
        (write-line "Saindo...")
        (write-line "")
        (return-from apagar_ordemservico 0)
    )

    (setq ordemservico_list (delete ordemservico ordemservico_list))

    (write-line "Executante apagado dos registros")
)

(defun editar_ordemservico()
    (cls)
    (format t "Digite o identificador do registro: ")
    (setq ordemservico_id (read))
    (setq ordemservico (nth ordemservico_id ordemservico_list))
    
    (when (not ordemservico)
        (write-line "Registro não encontrado")
        (return-from apagar_ordemservico 0)
    )


    (setq executante nil)
    (dolist (e executante_list)
        (if (string= (OrdemServico-executante ordemservico) (Executante-documento_pessoa e))
            (setq executante e)
        )
    )
    (setq solicitante nil)
    (dolist (s solicitante_list)
        (if (string= (OrdemServico-solicitante ordemservico) (Solicitante-documento_pessoa s))
            (setq solicitante s)
        )
    )


    (cls)
    (write-line "======[SISTEMA OS | EDIT. ORDEM DE SERVICO]======")
    (format t (concatenate 'string "Posição: " (write-to-string ordemservico_id)))
    
    (write-line "")
    (format t "Executante: ")
    (format t (Executante-documento_pessoa executante))

    (write-line "")
    (format t "Solicitante: ")
    (format t (Solicitante-documento_pessoa solicitante))

    
    (write-line "")
    (format t "Editar (0 = SIM | 1 = NÃO): ")
    (setq editar (read))

    (when (/= editar 0)
        (write-line "Saindo...")
        (write-line "")
        (return-from editar_ordemservico 0)
    )


    (setq ordemservico_lido (ler_ordemservico))

    (setf (nth ordemservico_id ordemservico_list) ordemservico_lido)

    (write-line "Ordem de servico editada com sucesso.")
)