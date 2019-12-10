/**
 * @license
 * Copyright (c) 2019 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/iron-icons/iron-icons.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/paper-toolbar/paper-toolbar.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-item/paper-item.js'
import './style-element.js';

class specificationUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<paper-dialog class="dialog" id="specificationUpdateModal" modal>
				<paper-toolbar>
					<div slot="top"><h2>Update Specification</h2></div>
				</paper-toolbar>
				<paper-input
						label="Name"
						value="{{specificationId}}"
						disabled>
				</paper-input>
				<paper-input
						label="Name"
						value="{{specificationName}}">
				</paper-input>
				<paper-input
						label="Description"
						value="{{specificationDescription}}">
				</paper-input>
				<paper-input
						label="Class"
						value="{{specificationType}}">
				</paper-input>
				<paper-input
						label="Base"
						value="{{specificationBase}}">
				</paper-input>
				<div class="buttons">
					<paper-button
							raised
							class="update-button"
							on-tap="_update">
						Update
					</paper-button>
					<paper-button
							class="cancel-button"
							on-tap="_cancel">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="specUpdateAjax"
					content-type="application/merge-patch+json"
					on-loading-changed="_onLoadingChanged"
					on-response="_response"
					on-error="_error">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			activeItem: {
				type: Object,
				observer: '_activeItemChanged'
			},
			specificationId: {
				type: String
			},
			specificationName: {
				type: String
			},
			specificationDescription: {
				type: String
			},
			specificationType: {
				type: String
			},
			specificationBase: {
				type: String
			},
			specificationChars: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item) {
		if(item) {
			this.specificationId = item.id;
			this.specificationName = item.name;
			this.specificationDescription = item.description;
			this.specificationType = item.type;
			this.specificationChars = item.chars;
			this.$.specificationUpdateModal.open();
		} else {
			this.specificationId = null;
			this.specificationName = null;
			this.specificationDescription = null;
			this.specificationType = null;
			this.specificationChars = [];
		}
	}

	_cancel() {
		this.$.specificationUpdateModal.close();
		this.specificationId = null;
		this.specificationName = null;
		this.specificationDescription = null;
		this.specificationType = null;
		this.specificationChars = [];
	}

	_update() {
		var ajax = this.$.specUpdateAjax;
		ajax.method = "PATCH";
		ajax.url = "/usageManagement/v4/usageSpecification/" + this.specificationId;
		var spec = new Array();
		if(this.specificationName) {
			var specName = new Object();
			specName.op = "add";
			specName.path = "/name";
			specName.value = this.specificationName;
			spec.push(specName);
		}
		if(this.specificationDescription) {
			var specDescription = new Object();
			specDescription.op = "replace";
			specDescription.path = "/description";
			specDescription.value = this.specificationDescription;
			spec.push(specDescription);
		}
		ajax.body = JSON.stringify(spec);
		ajax.generateRequest();
	}

	_response() {
		this.$.specificationUpdateModal.close();
		document.body.querySelector('usekeeper-shell').shadowRoot.getElementById('specificationList').shadowRoot.getElementById('specificationGrid').clearCache();
	}

	_error(event) {
		var toast = document.body.querySelector('usekeeper-shell').shadowRoot.getElementById('restError');
		toast.text = event.detail.request.xhr.statusText;
		toast.open();
	}
}

window.customElements.define('usekeeper-spec-update', specificationUpdate);
