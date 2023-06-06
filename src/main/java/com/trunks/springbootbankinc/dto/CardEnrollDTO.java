package com.trunks.springbootbankinc.dto;

import javax.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CardEnrollDTO {
	
	@Size(min = 10, max = 500, message = "Please provide a numeric carId with sixteen chars")
	private String cardId;

}
