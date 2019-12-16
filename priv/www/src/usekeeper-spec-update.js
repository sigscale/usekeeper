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
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-input/paper-textarea.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-item/paper-item.js'
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/iron-collapse/iron-collapse.js';
import './style-element.js';

class specificationUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<paper-dialog class="dialog" id="specificationUpdateModal" modal>
				<app-toolbar>
					<div main-title>Update Specification</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						label="Name"
						value="{{specificationId}}"
						disabled>
				</paper-input>
				<paper-input
						label="Name"
						value="{{specificationName}}">
				</paper-input>
				<paper-textarea
						label="Description"
						value="{{specificationDescription}}">
				</paper-textarea>
				<paper-input
						label="Class"
						value="{{specificationType}}">
				</paper-input>
				<paper-input
						label="Base"
						value="{{specificationBase}}">
				</paper-input>
				<div>
					<span>Characteristics</span>
						<paper-icon-button
							icon="arrow-drop-down"
							on-click="_collapseChars">
						</paper-icon-button>
				</div>
				<iron-collapse
						id="charSpecCollapse"
						opened="{{charSpecOpened}}">
					<template is="dom-repeat" items="[[specificationChars]]">
						<div>
							<hr>
							<paper-input
									label="Name"
									value="{{item.name}}"
									disabled>
							</paper-input>
							<paper-textarea
									label="Description"
									value="{{item.description}}">
							</paper-textarea>
							<paper-input
									label="ValueType"
									allowed-pattern="[a-z]"
									pattern="array|boolean|number|object|string"
									auto-validate
									error-message="invalid json type"
									value="{{item.valueType}}">
							</paper-input>
						</div>
					</template>
				</iron-collapse>
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
					<paper-button
							toggles
							raised
							class="delete-button"
							on-tap="_deleteSpec">
						Delete
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
				id="deleteSpecAjax"
				on_response="_deleteSpecResponse"
				on-error="_deleteSpecError">
			</iron-ajax>
			<iron-ajax
					id="specUpdateAjax"
					content-type="application/merge-patch+json"
					loading="{{loading}}"
					on-response="_response"
					on-error="_error">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			activeItem: {
				type: Object,
				observer: '_activeItemChanged'
			},
			charSpecOpened: {
				type: Boolean,
				observer: '_resize'
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
				type: Array
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
			if(this.$.charSpecCollapse.opened == true) {
				this.$.charSpecCollapse.hide();
			}
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
		if(this.$.charSpecCollapse.opened == true) {
			this.$.charSpecCollapse.hide();
		}
		this.specificationId = null;
		this.specificationName = null;
		this.specificationDescription = null;
		this.specificationType = null;
		this.specificationChars = [];
	}

	_deleteSpec() {
		var ajax1 = this.$.deleteSpecAjax;
		ajax1.method = "DELETE";
		ajax1.url = "/usageManagement/v4/usageSpecification/" + this.specificationId;
		ajax1.generateRequest();
		var deleteObj =  document.body.querySelector('usekeeper-shell').shadowRoot.querySelector('usekeeper-spec-update').shadowRoot.getElementById('specificationUpdateModal');
		deleteObj.close();
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

	_resize() {
		this.$.specificationUpdateModal.notifyResize();
	}

	_collapseChars(event) {
		if(this.$.charSpecCollapse.opened == false) {
			this.$.charSpecCollapse.show();
		} else {
			this.$.charSpecCollapse.hide();
		}
	}

}

window.customElements.define('usekeeper-spec-update', specificationUpdate);
