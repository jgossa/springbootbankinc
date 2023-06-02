package com.trunks.springbootbankinc.domain;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;

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
public class TransactionHistory implements Serializable {

	@Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="transactionhistory_id_sequence_generator")
    @SequenceGenerator(name="transactionhistory_id_sequence_generator", sequenceName="transactionhistory_id_sequence", allocationSize=1)
	private Long id;
	
	private Date date;
	
	private Float accountBalance;
	private TransactionType transactionType;
	
	private Boolean enroll;
	private Boolean block;
	
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "card_id")
	private Card card;
	
}
