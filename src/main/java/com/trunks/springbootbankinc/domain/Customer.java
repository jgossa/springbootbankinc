package com.trunks.springbootbankinc.domain;

import java.io.Serializable;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
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
public class Customer implements Serializable {

	@Id
    @GeneratedValue(strategy= GenerationType.SEQUENCE, generator="customer_id_sequence_generator")
    @SequenceGenerator(name="customer_id_sequence_generator", sequenceName="customer_id_sequence", allocationSize=1)
	private Long id;

	private String document;
	private String firstName;
	private String lastName;
	
	private DocumentType documentType;
	
	@OneToMany(mappedBy = "customer", cascade = CascadeType.ALL, orphanRemoval = true)
	private List<Card> cards;
}
