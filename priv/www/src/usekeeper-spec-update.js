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

class specUpdateList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element">
			</style>
			<paper-dialog class="dialog" id="updateSpecModal" modal>
				<paper-toolbar>
					<div slot="top"><h2>Update Specification</h2></div>
				</paper-toolbar>
				<paper-input
					id="updateSpecId"
					label="Name"
					value="{{specificationId}}"
					disabled>
				</paper-input>
				<paper-input
					id="updateSpecName"
					label="Name"
					value="{{specificationName}}">
				</paper-input>
				<paper-input
					id="updateSpecDesc"
					label="Description"
					value="{{specificationDescription}}">
				</paper-input>
				<paper-input
					id="updateSpecType"
					label="Class"
					value="{{specificationType}}">
				</paper-input>
				<paper-input
					id="updateSpecBase"
					label="Base"
					value="{{specificationBase}}">
				</paper-input>
				<div class="buttons">
					<paper-button
						raised
						class="update-button"
						on-tap="_updateSpec">
							Update
					</paper-button>
					<paper-button
						class="cancel-button"
						dialog-dismiss
						on-tap="cancelSpec">
							Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
				id="specUpdateAjax"
				content-type="application/merge-patch+json"
				on-loading-changed="_onLoadingChanged"
				on-response="_updateSpecResponse"
				on-error="_updateSpecError">
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
			this.$.updateSpecModal.open();
		} else {
			this.specificationId = null;
			this.specificationName = null;
			this.specificationDescription = null;
			this.specificationType = null;
			this.specificationChars = [];
		}
	}

	_cancel() {
		this.$.updateSpecModal.close();
		this.specificationId = null;
		this.specificationName = null;
		this.specificationDescription = null;
		this.specificationType = null;
		this.specificationChars = [];
	}

	_updateSpec() {
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
}

window.customElements.define('usekeeper-spec-update', specUpdateList);
