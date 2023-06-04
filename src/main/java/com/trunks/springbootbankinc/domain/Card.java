package com.trunks.springbootbankinc.domain;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Lob;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Transient;

import org.hibernate.annotations.Type;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Setter
@Getter
@Builder
@Entity
public class Card implements Serializable {
	
	@Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="card_id_sequence_generator")
    @SequenceGenerator(name="card_id_sequence_generator", sequenceName="card_id_sequence", allocationSize=1)

	private Long id;
    
	private String idProducto;
    private String number;
    
    private Date dueDate;
    private Date creationDate;
    
    private Float accountBalance;
    
    private Boolean enroll;
    private Boolean block;
    
	private TransactionType transactionType;
    
	@OneToMany(mappedBy = "card", fetch = FetchType.EAGER, cascade = CascadeType.ALL)
    private List<TransactionHistory> transactionHistories;
    
	@ManyToOne(fetch = FetchType.EAGER)
    private Customer customer;
	
	@Transient
	public void addTrx(TransactionHistory trx) {
		if(this.transactionHistories==null) {
			this.transactionHistories = new ArrayList<>();
		}
		this.transactionHistories.add(trx);
	}
}
