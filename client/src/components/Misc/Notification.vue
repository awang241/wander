<template>
  <div class="card colouredBackground" :style="cardStyle">
    <div class="columns">
      <div class="column">
        <p style="white-space: pre-wrap; font-weight:bolder" id="message">{{ notification.message }}</p>
        <p id="date">{{ dateTimeFormat(notification.timeStamp) }}</p>
      </div>
      <b-button
              v-if="notification.activityId !== null"
              id="viewButton"
              class="is-pulled-right"
              type="is-text"
              @click="goToActivity(notification.activityId)">
        View Activity
      </b-button>
    </div>
  </div>
</template>

<script>
  import toastMixin from "../../mixins/toastMixin";
  import store from "../../store";
  import router from "../../router";
  import dateTimeMixin from "../../mixins/dateTimeMixin";

  /**
   * Notification types that are classed as activity creations/additions and the associated colour green.
   * @type {{types: number[], colour: string}}
   */
  const ACTIVITY_ADDITIONS = {
    types: [
      'ACTIVITY_CREATED',
      'ACTIVITY_CREATOR_ADDED',
      'ACTIVITY_ORGANISER_ADDED',
      'ACTIVITY_FOLLOWER_ADDED',
      'ACTIVITY_PARTICIPANT_ADDED',
      'PARTICIPANT_CREATED'
    ],
    colour: "rgb(153,255,148, 0.2)"
  };

  /**
   * Notification types that are classed as removing/deleting activity/participation with associated colour red.
   * @type {{types: number[], colour: string}}
   */
  const ACTIVITY_REMOVALS = {
    types: [
      'ACTIVITY_REMOVED',
      'ACTIVITY_ORGANISER_REMOVED',
      'ACTIVITY_FOLLOWER_REMOVED',
      'ACTIVITY_PARTICIPANT_REMOVED'
    ],
    colour: "rgb(245,110,122, 0.2)"
  };

  /**
   * Notification types that are classed as member changes in colour yellow.
   * @type {{types: number[], colour: string}}
   */
  const ACTIVITY_CHANGES = {
    types: [
      'ACTIVITY_EDITED',
      'ACTIVITY_PRIVACY_CHANGED',
      'PARTICIPATION_EDITED'
    ],
    colour: "rgb(250,246,137, 0.2)"
  };

  /**
   * The colour for notifications caused by the currently logged in user.
   * @type {string}
   */
  const OWN_CHANGE_COLOUR = "rgb(250,246,137, 0.2)";

  /**
   * The default notification colour.
   * @type {string}
   */
  const DEFAULT_COLOUR = "#ffffff";

  Object.freeze(ACTIVITY_ADDITIONS);
  Object.freeze(ACTIVITY_REMOVALS);
  Object.freeze(ACTIVITY_CHANGES);


  export default {
    name: "Notification",
    mixins: [toastMixin, dateTimeMixin],
    data() {
      return {
        store: store,
        cardStyle: {
          backgroundColor: null
        }
      }
    },
    props: {
      notification: {
        type: Object,
        required: true
      }
    },
    /**
     * Sets the notification colour based on the notification type.
     */
    mounted() {
      if (this.isOwnNotification) {
        this.cardStyle.backgroundColor = OWN_CHANGE_COLOUR;
      } else if (ACTIVITY_ADDITIONS.types.includes(this.$props.notification.notificationType)) {
        this.cardStyle.backgroundColor = ACTIVITY_ADDITIONS.colour;
      } else if (ACTIVITY_REMOVALS.types.includes(this.$props.notification.notificationType)) {
        this.cardStyle.backgroundColor = ACTIVITY_REMOVALS.colour;
      } else if (ACTIVITY_CHANGES.types.includes(this.$props.notification.notificationType)) {
        this.cardStyle.backgroundColor = ACTIVITY_CHANGES.colour;
      } else {
        this.cardStyle.backgroundColor = DEFAULT_COLOUR
      }
    },
    methods: {
      /**
       * Given an activity's ID, routes to that activity's page.
       */
      goToActivity(id) {
        router.push({path: 'Activities/' + id})
      }
    },
    computed: {
      /**
       * Returns true if the notification was created by the currently logged in user; false otherwise
       */
      isOwnNotification() {
        return this.$props.notification.profileId === this.$store.getters.getUserId
      }
    }
  }
</script>

<style scoped>
  .card {
    padding: 1rem;
  }

  .colouredBackground{
    border-radius: 25px
  }

  #viewButton {
    margin: 1rem;
  }

</style>