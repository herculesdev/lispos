(in-package :user)

(defstruct Executante
    funcao
    isDisponivel
    ctps
    documento_pessoa
    )
(setq executante_list (list ))
  

(defun ler_executante()
    (cls)
    (format t "Digite o identificador da pessoa: ")
    (setq pessoa_id (read))
    (setq pessoa (nth pessoa_id pessoa_list))
        (when (not pessoa)
        (write-line "Registro não encontrado")
        (return-from ler_executante 0)
    )

    (format t "Função: ")
    (setq funcao-lido (read))

    (format t "Ctps: ")
    (setq ctps-lido (read))

    (format t "Ele está disponivel?(0-SIM / 1-NÃO) ")
    (setq opcao-lido(read))
    (if (= opcao-lido 0)
      (setq disponivel-lido 1)
      (setq disponivel-lido 0)
    )

    (setq e (make-Executante :funcao (write-to-string funcao-lido)
        :isDisponivel disponivel-lido :ctps ctps-lido
        :documento_pessoa(Pessoa-documento pessoa)))

    (return-from ler_executante e)
)

(defun cadastrar_executante ()
    (cls)
    (write-line "======[Sistema OS | CAD. EXECUTANTE]======")    
    (setq e (ler_executante))
    (push e executante_list)
    (write-line "")
    (write-line "")    
)

(defun listar_executantes ()
    (cls)
    (write-line "======[SISTEMA OS | LIST. EXECUTANTES]======")
    (write-line (concatenate 'string "Qtd. EXECUTANTES: " (write-to-string (list-length executante_list))))
    (write-line "")
    (setq contador 0)

    (dolist (item executante_list)
        (setq pessoa nil)
        (dolist (p pessoa_list)
            (if (string= (Executante-documento_pessoa item) (Pessoa-documento p))
                (setq pessoa p)
            )
        )
        (format t (concatenate 'string (write-to-string contador) " - "))
        (format t (Pessoa-nome pessoa))
        (format t ", ")
        (format t (Pessoa-documento pessoa))
        (format t ", Função: ")
        (format t (write-to-string (Executante-funcao item)))
        (format t ", CTPS: ")
        (format t (write-to-string (Executante-ctps item)))
        (format t ", Disponivel: ")
        (if (/= (Executante-isDisponivel item) 0)
          (format t "sim")
          (format t "não")
        )
        (write-line "")
        (setq contador (+ contador 1))
    )

    (write-line "")
)

(defun apagar_executante()
    (cls)
    (format t "Digite o identificador do registro: ")
    (setq executante_id (read))
    (setq executante (nth executante_id executante_list))
    
    (when (not executante)
        (write-line "Registro não encontrado")
        (return-from apagar_executante 0)
    )


    (setq pessoa nil)
    (dolist (pes pessoa_list)
        (if (string= (Executante-documento_pessoa executante) (Pessoa-documento pes))
            (setq pessoa pes)
        )
    )

    (cls)
    (write-line "======[SISTEMA OS | APAG. EXECUTANTE]======")
    (format t (concatenate 'string "Posição: " (write-to-string executante_id)))
    
    (write-line "")
    (format t "Nome: ")
    (format t (Pessoa-nome pessoa))

    (write-line "")
    (format t "Documento: ")
    (format t (Pessoa-documento pessoa))

    (write-line "")
    (format t "Função: ")
    (format t (write-to-string (Executante-funcao executante)))

    (write-line "")
    (format t "CTPS: ")
    (format t (write-to-string (Executante-ctps executante)))

    (write-line "")
    (format t "Disponivel: ")
    (if (/=  (Executante-isDisponivel executante) 0)
        (format t "sim")
        (format t "não")
    )

    
    (write-line "")
    (format t "Apagar (0 = SIM | 1 = NÃO): ")
    (setq editar (read))

    (when (/= editar 0)
        (write-line "Saindo...")
        (write-line "")
        (return-from apagar_executante 0)
    )

    (setq executante_list (delete executante executante_list))

    (write-line "Executante apagado dos registros")
)

(defun editar_executante()
    (cls)
    (format t "Digite o identificador do registro: ")
    (setq executante_id (read))
    (setq executante (nth executante_id executante_list))
    
    (when (not executante)
        (write-line "Registro não encontrado")
        (return-from editar_executante 0)
    )


    (setq pessoa nil)
    (dolist (pes pessoa_list)
        (if (string= (Executante-documento_pessoa executante) (Pessoa-documento pes))
            (setq pessoa pes)
        )
    )

    (cls)
    (write-line "======[SISTEMA OS | EDIT. EXECUTANTES]======")
    (format t (concatenate 'string "Posição: " (write-to-string executante_id)))
    
    (write-line "")
    (format t "Nome: ")
    (format t (Pessoa-nome pessoa))

    (write-line "")
    (format t "Documento: ")
    (format t (Pessoa-documento pessoa))

    (write-line "")
    (format t "Função: ")
    (format t (write-to-string (Executante-funcao executante)))

    (write-line "")
    (format t "CTPS: ")
    (format t (write-to-string (Executante-ctps executante)))

    (write-line "")
    (format t "Disponivel: ")
    (if (/=  (Executante-isDisponivel executante) 0)
        (format t "sim")
        (format t "não")
    )

    
    (write-line "")
    (format t "Editar (0 = SIM | 1 = NÃO): ")
    (setq editar (read))

    (when (/= editar 0)
        (write-line "Saindo...")
        (write-line "")
        (return-from editar_executante 0)
    )

    
    (setq executante_lido (ler_executante))

    (setf (nth executante_id executante_list) executante_lido)

    (write-line "Executante editado com sucesso.")
)