(in-package :user)

(defstruct Servico
    descricao
    valor
    os_numero
    )
(setq servico_list (list ))
  

(defun ler_servico()
    (cls)
    (format t "Digite o identificador da Ordem de Servico: ")
    (setq ordemservico_id (read))
    (setq ordemservico (nth ordemservico_id ordemservico_list))
        (when (not ordemservico)
        (write-line "Registro não encontrado")
        (return-from ler_servico 0)
    )

    (format t "Descrição: ")
    (setq descricao-lido (read-line))

    (format t "Valor: ")
    (setq valor-lido (read))

    (setq os_num (OrdemServico-numero ordemservico))

    (setq s (make-Servico :descricao (write-to-string descricao-lido) :valor valor-lido :os_numero os_num))
    (return-from ler_servico s)
)

(defun cadastrar_servico ()
    (cls)
    (write-line "======[Sistema OS | CAD. SERVICO]======")    
    (setq s (ler_servico))
    (push s servico_list)
    (write-line "")
    (write-line "")    
)

(defun listar_servicos ()
    (cls)
    (write-line "======[SISTEMA OS | LIST. SERVICO]======")
    (write-line (concatenate 'string "Qtd. SERVICO: " (write-to-string (list-length servico_list))))
    (write-line "")
    (setq contador 0)

    (dolist (item servico_list)
        (setq ordemservico nil)
        (dolist (os ordemservico_list)
            (if (= (OrdemServico-numero os) (Servico-os_numero item))
                (setq ordemservico os)
            )
        )
        (format t (concatenate 'string (write-to-string contador) " - Descricao: "))
        (format t (Servico-descricao item))
        (format t ", Valor: ")
        (format t (write-to-string (Servico-valor item)))
        (format t ", Numero OS: ")
        (format t (write-to-string (Servico-os_numero item)))
        (write-line "")
        (setq contador (+ contador 1))
    )

    (write-line "")
)

(defun apagar_servico()
    (format t "Digite o identificador do Servico: ")
    (setq servico_id (read))
    (setq servico (nth servico_id servico_list))
        (when (not servico)
        (write-line "Registro não encontrado")
        (return-from apagar_servico 0)
    )

    (dolist (os ordemservico_list)
        (if (string= (OrdemServico-numero os) (Servico-os_numero servico))
            (setq ordemservico os)
        )
    )

    (cls)
    (write-line "======[SISTEMA OS | APAG. SERVICO]======")
    (format t (concatenate 'string "Posição: " (write-to-string solicitante_id)))
    
    (write-line "")
    (format t "Descrição: ")
    (format t (Servico-descricao servico))

    (write-line "")
    (format t "Valor: ")
    (format t (Servico-valor servico))

    (write-line "")
    (format t "Número da OS: ")
    (format t (Servico-os_numero servico))

    (write-line "")
    (format t "Solicitante da OS: ")
    (format t (OrdemServico-solicitante ordemservico))

    (write-line "")
    (format t "Executante da OS: ")
    (format t (OrdemServico-executante ordemservico))
    
    (write-line "")
    (format t "Apagar (0 = SIM | 1 = NÃO): ")
    (setq editar (read))

    (when (/= editar 0)
        (write-line "Saindo...")
        (write-line "")
        (return-from apagar_servico 0)
    )

    (setq servico_list (delete servico servico_list))

    (write-line "Servico apagado dos registros")
)

(defun editar_servico()
    (format t "Digite o identificador do Servico: ")
    (setq servico_id (read))
    (setq servico (nth servico_id servico_list))
        (when (not servico)
        (write-line "Registro não encontrado")
        (return-from editar_servico 0)
    )

    (dolist (os ordemservico_list)
        (if (string= (OrdemServico-numero os) (Servico-os_numero servico))
            (setq ordemservico os)
        )
    )

    (cls)
    (write-line "======[SISTEMA OS | APAG. SERVICO]======")
    (format t (concatenate 'string "Posição: " (write-to-string servico_id)))
    
    (write-line "")
    (format t "Descrição: ")
    (format t (Servico-descricao servico))

    (write-line "")
    (format t "Valor: ")
    (format t (Servico-valor servico))

    (write-line "")
    (format t "Número da OS: ")
    (format t (Servico-os_numero servico))

    (write-line "")
    (format t "Solicitante da OS: ")
    (format t (OrdemServico-solicitante ordemservico))

    (write-line "")
    (format t "Executante da OS: ")
    (format t (OrdemServico-executante ordemservico))

    
    (write-line "")
    (format t "Editar (0 = SIM | 1 = NÃO): ")
    (setq editar (read))

    (when (/= editar 0)
        (write-line "Saindo...")
        (write-line "")
        (return-from editar_servico 0)
    )

    
    (setq servico_lido (ler_servico))

    (setf (nth servico_id servico_list) servico_lido)

    (write-line "Servico editado com sucesso.")
)